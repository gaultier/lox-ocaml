open Parse
open Base

type scope = (string, bool) Hashtbl.t [@@deriving sexp_of]

type scopes = scope Stack.t [@@deriving sexp_of]

type resolution = (id, int, Int.comparator_witness) Map.t

type var_id_to_name = (id, string, Int.comparator_witness) Map.t

type resolution_context = {
  scopes : scopes;
  resolution : resolution;
  current_fn_type : unit option;
  var_id_to_name : var_id_to_name;
}

let remove_used_vars ctx =
  Stack.fold ~init:0
    ~f:(fun depth scope ->
      Map.filter ~f:(Int.equal depth) ctx.resolution
      |> Map.keys
      |> List.map ~f:(Map.find_exn ctx.var_id_to_name)
      |> List.iter ~f:(Hashtbl.remove scope);
      depth + 1)
    ctx.scopes
  |> ignore

let print_scopes = Stack.iter ~f:(Hashtbl.iter_keys ~f:Stdlib.print_endline)

let new_scope () : scope = Hashtbl.create (module String)

let print_resolution (resolution : resolution) =
  Map.iteri
    ~f:(fun ~key:k ~data:d -> Stdlib.Printf.printf "- %d: %d\n" k d)
    resolution

let declare_var ctx name =
  Stack.top ctx.scopes
  |> Option.iter ~f:(fun scope ->
         match Hashtbl.add scope ~key:name ~data:false with
         | `Ok -> ()
         | `Duplicate ->
             Printf.failwithf
               "Forbidden shadowing of variable `%s` in the same scope" name ());
  ctx

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
  {
    ctx with
    resolution = Map.add_exn ctx.resolution ~key:id ~data:depth;
    var_id_to_name = Map.add_exn ~key:id ~data:n ctx.var_id_to_name;
  }

let rec resolve_function ctx (args : Lex.token list) (stmts : statement list) =
  Stack.push ctx.scopes (new_scope ());
  let ctx =
    List.fold ~init:ctx
      ~f:(fun ctx arg ->
        match arg with
        | { kind = Identifier n; _ } ->
            let ctx = declare_var ctx n in
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
      let ctx =
        Array.fold ~f:(fun ctx stmt -> resolve_stmt ctx stmt) ~init:ctx stmts
      in
      Stack.pop_exn ctx.scopes |> ignore;
      ctx
  | Var (Lex.Identifier n, expr, _) ->
      let ctx = declare_var ctx n in
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
      let ctx = declare_var ctx name in
      let ctx = define_var ctx name in
      let ctx =
        {
          ctx with
          var_id_to_name = Map.add_exn ~key:id ~data:name ctx.var_id_to_name;
        }
      in
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
        var_id_to_name = Map.empty (module Int);
        scopes = Stack.create ();
        current_fn_type = None;
      }
    in
    Stack.push ctx.scopes (new_scope ());
    let ctx = resolve_stmts ctx stmts in
    Stack.pop_exn ctx.scopes |> ignore;
    print_scopes ctx.scopes;
    Ok (stmts, ctx.resolution)
  with Failure err -> Result.Error [ err ]
