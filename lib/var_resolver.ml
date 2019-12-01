open Parse
open Base

type scope = (string, bool) Hashtbl.t [@@deriving sexp_of]

type scope_var_name_to_id = (string, id) Hashtbl.t [@@deriving sexp_of]

type scopes = scope Stack.t [@@deriving sexp_of]

type scopes_var_name_to_id = scope_var_name_to_id Stack.t

type resolution = (id, int, Int.comparator_witness) Map.t

type var_ids = (id, Int.comparator_witness) Set.t

type resolution_context = {
  scopes : scopes;
  resolution : resolution;
  current_fn_type : unit option;
  scopes_var_name_to_id : scopes_var_name_to_id;
  var_ids : var_ids;
}

let print_scopes = Stack.iter ~f:(Hashtbl.iter_keys ~f:Stdlib.print_endline)

let print_scopes_var_name_to_id scopes_var_name_to_id =
  Stack.iter
    ~f:(fun s ->
      Hashtbl.iteri
        ~f:(fun ~key:n ~data:id -> Stdlib.Printf.printf "- %s: %d\n" n id)
        s)
    scopes_var_name_to_id

let new_scope () : scope = Hashtbl.create (module String)

let new_scope_var_name_to_id () : scope_var_name_to_id =
  Hashtbl.create (module String)

let print_resolution (resolution : resolution) =
  Map.iteri
    ~f:(fun ~key:k ~data:d -> Stdlib.Printf.printf "- %d: %d\n" k d)
    resolution

let declare_var ctx name (id : id) =
  Stack.top ctx.scopes
  |> Option.iter ~f:(fun scope ->
         match Hashtbl.add scope ~key:name ~data:false with
         | `Ok -> ()
         | `Duplicate ->
             Printf.failwithf
               "Forbidden shadowing of variable `%s` in the same scope" name ());
  Stack.top_exn ctx.scopes_var_name_to_id |> Hashtbl.add_exn ~key:name ~data:id;
  { ctx with var_ids = Set.add ctx.var_ids id }

let define_var ctx name =
  Stack.top ctx.scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:true);
  ctx

let resolve_local ctx id n =
  let depth =
    Stack.fold_until ~init:0
      ~f:(fun depth scope ->
        match Hashtbl.find scope n with
        | Some _ -> Stop depth
        | None -> Continue (depth + 1))
      ~finish:(fun depth -> depth)
      ctx.scopes
  in

  let var_id =
    Stack.fold_until ~init:None
      ~f:(fun _ scope ->
        match Hashtbl.find scope n with
        | Some _ as s -> Stop s
        | None -> Continue None)
      ~finish:(fun x -> x)
      ctx.scopes_var_name_to_id
  in
  Stdlib.Printf.printf "resolve_local: n=%s expr id=%d original id=%d d=%d\n" n
    id
    (Option.value ~default:(-1) var_id)
    depth;

  print_scopes_var_name_to_id ctx.scopes_var_name_to_id;
  {
    ctx with
    resolution = Map.add_exn ctx.resolution ~key:id ~data:depth;
    var_ids =
      ( match var_id with
      | Some id -> Set.remove ctx.var_ids id
      | None -> ctx.var_ids );
  }

let rec resolve_function ctx (args : Lex.token list) (stmts : statement list) =
  Stack.push ctx.scopes (new_scope ());
  let ctx =
    List.fold ~init:ctx
      ~f:(fun ctx arg ->
        match arg with
        | { kind = Identifier n; _ } ->
            let ctx = declare_var ctx n 0 in
            define_var ctx n
        | { kind; _ } ->
            Printf.failwithf "Invalid function argument: %s "
              (kind |> Lex.sexp_of_token_kind |> Sexp.to_string_hum)
              ())
      args
  in
  let ctx = resolve_stmts { ctx with current_fn_type = Some () } stmts in

  Stack.pop_exn ctx.scopes |> ignore;
  ctx

and resolve_expr ctx = function
  | Assign (Lex.Identifier n, expr, id) ->
      let ctx = resolve_expr ctx expr in
      resolve_local ctx id n
  | Variable (Lex.Identifier n, id) ->
      Stack.top ctx.scopes
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope n)
      |> Option.iter ~f:(fun b ->
             if Bool.equal b false then
               Printf.failwithf
                 "Cannot read variable `%s` in its own initializer" n ());
      resolve_local ctx id n
  | Call (callee, _, args, _) ->
      let ctx = resolve_expr ctx callee in
      List.fold ~init:ctx ~f:(fun ctx arg -> resolve_expr ctx arg) args
  | Binary (left, _, right, _)
  | LogicalOr (left, right, _)
  | LogicalAnd (left, right, _) ->
      let ctx = resolve_expr ctx left in
      resolve_expr ctx right
  | Unary (_, e, _) | Grouping (e, _) -> resolve_expr ctx e
  | Literal _ -> ctx
  | Assign _ as a ->
      Printf.failwithf "Invalid assignment: %s "
        (a |> sexp_of_expr |> Sexp.to_string_hum)
        ()
  | Variable _ as v ->
      Printf.failwithf "Invalid variable: %s "
        (v |> sexp_of_expr |> Sexp.to_string_hum)
        ()

and resolve_stmt ctx = function
  | Block (stmts, _) ->
      Stack.push ctx.scopes (new_scope ());
      Stack.push ctx.scopes_var_name_to_id (new_scope_var_name_to_id ());
      let ctx =
        Array.fold ~f:(fun ctx stmt -> resolve_stmt ctx stmt) ~init:ctx stmts
      in
      Stack.pop_exn ctx.scopes |> ignore;
      Stack.pop_exn ctx.scopes_var_name_to_id |> ignore;
      ctx
  | Var (Lex.Identifier n, expr, id) ->
      let ctx = declare_var ctx n id in
      let ctx = resolve_expr ctx expr in
      define_var ctx n
  | Print (e, _) | Expr (e, _) -> resolve_expr ctx e
  | Return (_, e, _) -> (
      match ctx.current_fn_type with
      | None ->
          Printf.failwithf
            "Cannot return outside of a function body. Returning: `%s`"
            (e |> sexp_of_expr |> Sexp.to_string_hum)
            ()
      | Some _ -> resolve_expr ctx e )
  | Function ({ Lex.kind = Lex.Identifier name; _ }, args, stmts, id) ->
      let ctx = declare_var ctx name id in
      let ctx = define_var ctx name in
      resolve_function ctx args stmts
  | WhileStmt (e, stmt, _) | IfStmt (e, stmt, _) ->
      let ctx = resolve_expr ctx e in
      resolve_stmt ctx stmt
  | IfElseStmt (e, then_stmt, else_stmt, _) ->
      let ctx = resolve_expr ctx e in
      let ctx = resolve_stmt ctx then_stmt in
      resolve_stmt ctx else_stmt
  | Function _ as f ->
      Printf.failwithf "Invalid function declaration: %s "
        (f |> sexp_of_statement |> Sexp.to_string_hum)
        ()
  | Var _ as v ->
      Printf.failwithf "Invalid variable declaration: %s "
        (v |> sexp_of_statement |> Sexp.to_string_hum)
        ()

and resolve_stmts ctx (stmts : statement list) =
  List.fold ~f:(fun ctx stmt -> resolve_stmt ctx stmt) ~init:ctx stmts

let resolve stmts =
  try
    let ctx =
      {
        resolution = Map.empty (module Int);
        var_ids = Set.empty (module Int);
        scopes_var_name_to_id = Stack.create ();
        scopes = Stack.create ();
        current_fn_type = None;
      }
    in
    Stack.push ctx.scopes (new_scope ());
    Stack.push ctx.scopes_var_name_to_id (new_scope_var_name_to_id ());
    let ctx = resolve_stmts ctx stmts in
    Stack.pop_exn ctx.scopes |> ignore;
    Set.iter
      ~f:(fun (id : id) -> Stdlib.Printf.printf "unused: %d\n" id)
      ctx.var_ids;
    Ok (stmts, ctx.resolution)
  with Failure err -> Result.Error [ err ]
