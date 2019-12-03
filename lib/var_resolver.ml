open Parse
open Base

type scope = { vars_status : (string, bool) Hashtbl.t; block_id : id }
[@@deriving sexp_of]

type scopes = scope Stack.t [@@deriving sexp_of]

type resolution = (id, int, Int.comparator_witness) Map.t

type var = string * id [@@deriving sexp_of]

type vars = var list

type resolution_context = {
  scopes : scopes;
  resolution : resolution;
  current_fn_type : unit option;
  vars : vars;
}

let new_scope block_id : scope =
  { vars_status = Hashtbl.create (module String); block_id }

let print_resolution (resolution : resolution) =
  Map.iteri
    ~f:(fun ~key:k ~data:d -> Stdlib.Printf.printf "- %d: %d\n" k d)
    resolution

let declare_var name ctx =
  let scope = Stack.top_exn ctx.scopes in
  let _ =
    match Hashtbl.add scope.vars_status ~key:name ~data:false with
    | `Ok -> ()
    | `Duplicate ->
        Printf.failwithf
          "Forbidden shadowing of variable `%s` in the same scope" name ()
  in
  { ctx with vars = (name, scope.block_id) :: ctx.vars }

let define_var name ctx =
  let scope = Stack.top_exn ctx.scopes in
  Hashtbl.set scope.vars_status ~key:name ~data:true;
  ctx

let mark_var_as_used name depth ctx =
  let scopes = Stack.copy ctx.scopes in
  for _ = 0 to depth - 1 do
    Stack.pop scopes |> ignore
  done;
  let vars =
    Stack.pop scopes
    |> Option.map ~f:(fun scope ->
           List.filter
             ~f:(fun (n, b_id) ->
               not (String.equal name n && scope.block_id = b_id))
             ctx.vars)
    |> Option.value ~default:ctx.vars
  in

  { ctx with vars }

let resolve_local id n ctx =
  let depth =
    Stack.fold_until ~init:0
      ~f:(fun depth scope ->
        match Hashtbl.find scope.vars_status n with
        | Some _ -> Stop depth
        | None -> Continue (depth + 1))
      ~finish:(fun depth -> depth)
      ctx.scopes
  in

  { ctx with resolution = Map.add_exn ctx.resolution ~key:id ~data:depth }
  |> mark_var_as_used n depth

let rec resolve_function ctx (args : Lex.token list) (stmts : statement list)
    fn_id =
  Stack.push ctx.scopes (new_scope fn_id);
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
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope.vars_status n)
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
      Stack.push ctx.scopes (new_scope id);
      let ctx = Array.fold ~f:resolve_stmt_ ~init:ctx stmts in
      Stack.pop_exn ctx.scopes |> ignore;
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
      let ctx = resolve_function ctx args stmts id in
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
      }
    in
    Stack.push ctx.scopes (new_scope 0);
    let ctx = resolve_stmts ctx stmts in
    Stack.pop_exn ctx.scopes |> ignore;
    List.iter
      ~f:(fun (n, _) -> Stdlib.Printf.printf "Unused variable: %s\n" n)
      ctx.vars;
    Ok (stmts, ctx.resolution)
  with Failure err -> Result.Error [ err ]
