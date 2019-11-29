open Parse
open Base

type scope = (string, bool) Hashtbl.t [@@deriving sexp_of]

type scopes = scope Stack.t [@@deriving sexp_of]

type unused_vars = (id, Int.comparator_witness) Set.t

type resolution = (id, int, Int.comparator_witness) Map.t

let new_scope () : scope = Hashtbl.create (module String)

let print_resolution (resolution : resolution) =
  Map.iteri
    ~f:(fun ~key:k ~data:d -> Stdlib.Printf.printf "- %d: %d\n" k d)
    resolution

let declare_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope ->
         match Hashtbl.add scope ~key:name ~data:false with
         | `Ok -> ()
         | `Duplicate ->
             Printf.failwithf
               "Forbidden shadowing of variable `%s` in the same scope" name ())

let define_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:true)

let resolve_local (resolution : resolution) (scopes : scopes) expr n =
  let depth =
    Stack.fold_until ~init:0
      ~f:(fun depth scope ->
        match Hashtbl.find scope n with
        | Some _ -> Stop depth
        | None -> Continue (depth + 1))
      ~finish:(fun depth -> depth)
      scopes
  in
  Map.add_exn resolution ~key:expr ~data:depth

let rec resolve_function unused_vars (resolution : resolution) (scopes : scopes)
    (args : Lex.token list) (stmts : statement list) =
  Stack.push scopes (new_scope ());
  List.iter
    ~f:(fun arg ->
      match arg with
      | { kind = Identifier n; _ } ->
          declare_var scopes n;
          define_var scopes n
      | { kind; _ } ->
          Printf.failwithf "Invalid function argument: %s "
            (kind |> Lex.sexp_of_token_kind |> Sexp.to_string_hum)
            ())
    args;
  let resolution =
    resolve_stmts unused_vars resolution scopes (Some ()) stmts
  in
  Stack.pop_exn scopes |> ignore;
  resolution

and resolve_expr unused_vars (resolution : resolution) (scopes : scopes) =
  function
  | Assign (Lex.Identifier n, expr, id) ->
      let resolution = resolve_expr unused_vars resolution scopes expr in
      resolve_local resolution scopes id n
  | Variable (Lex.Identifier n, id) ->
      Stack.top scopes
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope n)
      |> Option.iter ~f:(fun b ->
             if Bool.equal b false then
               Printf.failwithf
                 "Cannot read variable `%s` in its own initializer" n ());
      resolve_local resolution scopes id n
  | Call (callee, _, args, _) ->
      let resolution = resolve_expr unused_vars resolution scopes callee in
      let resolution =
        List.fold ~init:resolution
          ~f:(fun resolution arg ->
            resolve_expr unused_vars resolution scopes arg)
          args
      in
      resolution
  | Binary (left, _, right, _)
  | LogicalOr (left, right, _)
  | LogicalAnd (left, right, _) ->
      let resolution = resolve_expr unused_vars resolution scopes left in
      resolve_expr unused_vars resolution scopes right
  | Unary (_, e, _) | Grouping (e, _) ->
      resolve_expr unused_vars resolution scopes e
  | Literal _ -> resolution
  | Assign _ as a ->
      Printf.failwithf "Invalid assignment: %s "
        (a |> sexp_of_expr |> Sexp.to_string_hum)
        ()
  | Variable _ as v ->
      Printf.failwithf "Invalid variable: %s "
        (v |> sexp_of_expr |> Sexp.to_string_hum)
        ()

and resolve_stmt unused_vars (resolution : resolution) (scopes : scopes)
    (current_fn_type : unit option) = function
  | Block (stmts, _) ->
      Stack.push scopes (new_scope ());
      let resolution =
        Array.fold
          ~f:(fun resolution stmt ->
            resolve_stmt unused_vars resolution scopes current_fn_type stmt)
          ~init:resolution stmts
      in
      Stack.pop_exn scopes |> ignore;
      resolution
  | Var (Lex.Identifier n, expr, _) ->
      declare_var scopes n;
      let resolution = resolve_expr unused_vars resolution scopes expr in
      define_var scopes n;
      resolution
  | Print (e, _) | Expr (e, _) -> resolve_expr unused_vars resolution scopes e
  | Return (_, e, _) -> (
      match current_fn_type with
      | None ->
          Printf.failwithf
            "Cannot return outside of a function body. Returning: `%s`"
            (e |> sexp_of_expr |> Sexp.to_string_hum)
            ()
      | Some _ -> resolve_expr unused_vars resolution scopes e )
  | Function ({ Lex.kind = Lex.Identifier name; _ }, args, stmts, _) ->
      declare_var scopes name;
      define_var scopes name;
      resolve_function unused_vars resolution scopes args stmts
  | WhileStmt (e, stmt, _) | IfStmt (e, stmt, _) ->
      let resolution = resolve_expr unused_vars resolution scopes e in
      resolve_stmt unused_vars resolution scopes current_fn_type stmt
  | IfElseStmt (e, then_stmt, else_stmt, _) ->
      let resolution = resolve_expr unused_vars resolution scopes e in
      let resolution =
        resolve_stmt unused_vars resolution scopes current_fn_type then_stmt
      in
      resolve_stmt unused_vars resolution scopes current_fn_type else_stmt
  | Function _ as f ->
      Printf.failwithf "Invalid function declaration: %s "
        (f |> sexp_of_statement |> Sexp.to_string_hum)
        ()
  | Var _ as v ->
      Printf.failwithf "Invalid variable declaration: %s "
        (v |> sexp_of_statement |> Sexp.to_string_hum)
        ()

and resolve_stmts unused_vars (resolution : resolution) (scopes : scopes)
    (current_fn_type : unit option) (stmts : statement list) =
  List.fold
    ~f:(fun resolution stmt ->
      resolve_stmt unused_vars resolution scopes current_fn_type stmt)
    ~init:resolution stmts

let resolve stmts =
  try
    let resolution : resolution = Map.empty (module Int) in
    let unused_vars : unused_vars = Set.empty (module Int) in
    let scopes : scopes = Stack.create () in
    Stack.push scopes (new_scope ());
    let resolution = resolve_stmts unused_vars resolution scopes None stmts in
    Stack.pop_exn scopes |> ignore;
    Ok (stmts, resolution)
  with Failure err -> Result.Error [ err ]
