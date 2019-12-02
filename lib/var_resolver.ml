open Parse
open Base

type scope = (string, bool) Hashtbl.t [@@deriving sexp_of]

type scopes = scope Stack.t [@@deriving sexp_of]

type block_ids = id Stack.t

type resolution = (id, int, Int.comparator_witness) Map.t

type var = string * id [@@deriving sexp_of]

type vars = var list

type resolution_context = {
  scopes : scopes;
  resolution : resolution;
  current_fn_type : unit option;
  vars : vars;
  block_ids : block_ids;
}

let new_scope () : scope = Hashtbl.create (module String)

let print_resolution (resolution : resolution) =
  Map.iteri
    ~f:(fun ~key:k ~data:d -> Stdlib.Printf.printf "- %d: %d\n" k d)
    resolution

let declare_var name ctx =
  Stack.top ctx.scopes
  |> Option.iter ~f:(fun scope ->
         match Hashtbl.add scope ~key:name ~data:false with
         | `Ok -> ()
         | `Duplicate ->
             Printf.failwithf
               "Forbidden shadowing of variable `%s` in the same scope" name ());
  let current_block_id = Stack.top_exn ctx.block_ids in
  { ctx with vars = (name, current_block_id) :: ctx.vars }

let define_var name ctx =
  Stack.top ctx.scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:true);
  ctx

let mark_var_as_used name depth ctx =
  let block_ids = Stack.copy ctx.block_ids in
  for _ = 0 to depth - 1 do
    Stack.pop block_ids |> ignore
  done;
  let vars =
    Stack.pop block_ids
    |> Option.map ~f:(fun block_id ->
           List.filter
             ~f:(fun (n, b_id) -> not (String.equal name n && block_id = b_id))
             ctx.vars)
    |> Option.value ~default:ctx.vars
  in

  { ctx with vars }

let resolve_local id n ctx =
  let depth =
    Stack.fold_until ~init:0
      ~f:(fun depth scope ->
        match Hashtbl.find scope n with
        | Some _ -> Stop depth
        | None -> Continue (depth + 1))
      ~finish:(fun depth -> depth)
      ctx.scopes
  in

  { ctx with resolution = Map.add_exn ctx.resolution ~key:id ~data:depth }
  |> mark_var_as_used n depth

let rec resolve_function ctx (args : Lex.token list) (stmts : statement list) =
  Stack.push ctx.scopes (new_scope ());
  let ctx =
    List.fold ~init:ctx
      ~f:(fun ctx arg ->
        match arg with
        | { kind = Identifier n; _ } -> ctx |> declare_var n |> define_var n
        | { kind; _ } ->
            Printf.failwithf "Invalid function argument: %s "
              (kind |> Lex.sexp_of_token_kind |> Sexp.to_string_hum)
              ())
      args
  in
  let ctx = resolve_stmts { ctx with current_fn_type = Some () } stmts in

  Stack.pop_exn ctx.scopes |> ignore;
  ctx

and resolve_expr_ ctx = function
  | Assign (Lex.Identifier n, expr, id) ->
      ctx |> resolve_expr expr |> resolve_local id n
  | Variable (Lex.Identifier n, id) ->
      Stack.top ctx.scopes
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope n)
      |> Option.iter ~f:(fun b ->
             if Bool.equal b false then
               Printf.failwithf
                 "Cannot read variable `%s` in its own initializer" n ());
      resolve_local id n ctx
  | Call (callee, _, args, _) ->
      let ctx = resolve_expr callee ctx in
      List.fold ~init:ctx ~f:resolve_expr_ args
  | Binary (left, _, right, _)
  | LogicalOr (left, right, _)
  | LogicalAnd (left, right, _) ->
      ctx |> resolve_expr left |> resolve_expr right
  | Unary (_, e, _) | Grouping (e, _) -> resolve_expr e ctx
  | Literal _ -> ctx
  | Assign _ as a ->
      Printf.failwithf "Invalid assignment: %s "
        (a |> sexp_of_expr |> Sexp.to_string_hum)
        ()
  | Variable _ as v ->
      Printf.failwithf "Invalid variable: %s "
        (v |> sexp_of_expr |> Sexp.to_string_hum)
        ()

and resolve_stmt_ ctx = function
  | Block (stmts, id) ->
      Stack.push ctx.scopes (new_scope ());
      Stack.push ctx.block_ids id;
      let ctx = Array.fold ~f:resolve_stmt_ ~init:ctx stmts in
      Stack.pop_exn ctx.scopes |> ignore;
      Stack.pop_exn ctx.block_ids |> ignore;
      ctx
  | Var (Lex.Identifier n, expr, _) ->
      ctx |> declare_var n |> resolve_expr expr |> define_var n
  | Print (e, _) | Expr (e, _) -> resolve_expr e ctx
  | Return (_, e, _) -> (
      match ctx.current_fn_type with
      | None ->
          Printf.failwithf
            "Cannot return outside of a function body. Returning: `%s`"
            (e |> sexp_of_expr |> Sexp.to_string_hum)
            ()
      | Some _ -> resolve_expr e ctx )
  | Function ({ Lex.kind = Lex.Identifier name; _ }, args, stmts, id) ->
      let ctx = ctx |> declare_var name |> define_var name in
      Stack.push ctx.block_ids id;
      let ctx = resolve_function ctx args stmts in
      Stack.pop_exn ctx.block_ids |> ignore;
      ctx
  | WhileStmt (e, stmt, _) | IfStmt (e, stmt, _) ->
      ctx |> resolve_expr e |> resolve_stmt stmt
  | IfElseStmt (e, then_stmt, else_stmt, _) ->
      ctx |> resolve_expr e |> resolve_stmt then_stmt |> resolve_stmt else_stmt
  | Function _ as f ->
      Printf.failwithf "Invalid function declaration: %s "
        (f |> sexp_of_statement |> Sexp.to_string_hum)
        ()
  | Var _ as v ->
      Printf.failwithf "Invalid variable declaration: %s "
        (v |> sexp_of_statement |> Sexp.to_string_hum)
        ()

and resolve_stmt stmt ctx = resolve_stmt_ ctx stmt

and resolve_expr expr ctx = resolve_expr_ ctx expr

and resolve_stmts ctx (stmts : statement list) =
  List.fold ~f:resolve_stmt_ ~init:ctx stmts

let resolve stmts =
  try
    let ctx =
      {
        resolution = Map.empty (module Int);
        vars = [];
        scopes = Stack.create ();
        current_fn_type = None;
        block_ids = Stack.create ();
      }
    in
    Stack.push ctx.scopes (new_scope ());
    Stack.push ctx.block_ids 0;
    let ctx = resolve_stmts ctx stmts in
    Stack.pop_exn ctx.scopes |> ignore;
    List.iter
      ~f:(fun (n, _) -> Stdlib.Printf.printf "Unused variable: %s\n" n)
      ctx.vars;
    Ok (stmts, ctx.resolution)
  with Failure err -> Result.Error [ err ]
