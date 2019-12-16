open Parse
open Base
open Base.Option.Let_syntax

type function_type = Function | Method

type scope = { vars_status : (string, bool) Hashtbl.t; block_id : id }

type scopes = scope Stack.t

type resolution = (id, int, Int.comparator_witness) Map.t

let make_resolution () = Map.empty (module Int)

type var = string * id

type vars = var list

type resolution_context = {
  scopes : scopes;
  resolution : resolution;
  current_fn_type : function_type option;
  vars : vars;
}

let opt_value ~error x =
  Option.value_exn ~error:(Error.of_exn (Failure error)) x

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

let resolve_local id name ctx =
  let depth, block_id =
    Stack.fold_until ~init:(0, None)
      ~f:(fun (depth, _) scope ->
        match Hashtbl.find scope.vars_status name with
        | Some _ -> Stop (depth, Some scope.block_id)
        | None -> Continue (depth + 1, None))
      ~finish:Fn.id ctx.scopes
  in
  let%map block_id = block_id in
  {
    ctx with
    resolution = Map.add_exn ctx.resolution ~key:id ~data:depth;
    vars =
      List.filter
        ~f:(fun (n, b_id) -> not (String.equal name n && block_id = b_id))
        ctx.vars;
  }

let resolve_class id ctx =
  (* Stack.push ctx.scopes (new_scope id); *)
  (* Stack.pop_exn ctx.scopes |> ignore; *)
  { ctx with resolution = Map.add_exn ctx.resolution ~key:id ~data:0 }

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
  let ctx = resolve_stmts { ctx with current_fn_type = Some Function } stmts in

  Stack.pop_exn ctx.scopes |> ignore;
  ctx

and resolve_expr_ ctx = function
  | This ({ lines; columns; _ }, id) ->
      resolve_local id "this" ctx
      |> opt_value ~error:(Printf.sprintf "%d:%d:Unbound `this`" lines columns)
  | Get (e, _) -> resolve_expr_ ctx e
  | Set (lhs, _, rhs) -> ctx |> resolve_expr lhs |> resolve_expr rhs
  | Assign (Lex.Identifier n, expr, id) ->
      ctx |> resolve_expr expr |> resolve_local id n
      |> opt_value ~error:(Printf.sprintf "Assigning unbound variable %s" n)
  | Variable (Lex.Identifier n, id) ->
      Stack.top ctx.scopes
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope.vars_status n)
      |> Option.iter ~f:(fun b ->
             if Bool.equal b false then
               Printf.failwithf
                 "Cannot read variable `%s` in its own initializer" n ());
      resolve_local id n ctx
      |> opt_value ~error:(Printf.sprintf "Accessing unbound variable %s " n)
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
  | Class (n, methods, id) ->
      let ctx = ctx |> declare_var n |> define_var n |> resolve_class id in
      let scope = new_scope id in
      Hashtbl.set ~key:"this" ~data:true scope.vars_status;
      Stack.push ctx.scopes scope;

      let ctx =
        List.fold ~init:ctx
          ~f:(fun ctx m ->
            match m with
            | Function ({ Lex.kind = Lex.Identifier _; _ }, args, stmts, id) ->
                resolve_function
                  { ctx with current_fn_type = Some Method }
                  args stmts id
            | _ -> failwith "Malformed method")
          methods
      in
      Stack.pop ctx.scopes |> ignore;
      ctx
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

let make_ctx resolution =
  let ctx =
    { resolution; vars = []; scopes = Stack.create (); current_fn_type = None }
  in
  Stack.push ctx.scopes (new_scope 0);
  let scope = new_scope 0 in
  Hashtbl.iter_keys Parse.globals.values ~f:(fun n ->
      Hashtbl.add_exn ~key:n ~data:true scope.vars_status);
  Stack.push ctx.scopes scope;
  ctx

let resolve ~resolution ~check_unused stmts =
  try
    let ctx = make_ctx resolution in
    let ctx = resolve_stmts ctx stmts in
    Stack.pop_exn ctx.scopes |> ignore;
    if check_unused then
      List.iter
        ~f:(fun (n, _) -> Stdlib.Printf.printf "Unused variable: %s\n" n)
        ctx.vars;
    Ok (stmts, ctx.resolution)
  with Failure err -> Result.Error [ err ]
