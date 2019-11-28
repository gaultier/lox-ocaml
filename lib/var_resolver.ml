open Parse
open Base
open Base.Result.Let_syntax

type scope = (string, bool) Hashtbl.t [@@deriving sexp_of]

type scopes = scope Stack.t [@@deriving sexp_of]

type resolution = (id, int, Int.comparator_witness) Map.t

let new_scope () : scope = Hashtbl.create (module String)

let print_resolution (resolution : resolution) =
  Map.iteri
    ~f:(fun ~key:k ~data:d -> Stdlib.Printf.printf "- %d: %d\n" k d)
    resolution

let declare_var scopes name =
  let%bind scope =
    Stack.top scopes |> Result.of_option ~error:[ "Expected scope" ]
  in
  match Hashtbl.add scope ~key:name ~data:false with
  | `Ok -> Result.ok_unit
  | `Duplicate ->
      Result.fail
        [
          Printf.sprintf
            "Forbidden shadowing of variable `%s` in the same scope" name;
        ]

let define_var scopes name =
  Stack.top scopes
  |> Option.map ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:true)
  |> Result.of_option ~error:[ "Expected scope" ]

let resolve_local (resolution : resolution) (scopes : scopes) id n =
  let depth =
    Stack.fold_until ~init:0
      ~f:(fun depth scope ->
        match Hashtbl.find scope n with
        | Some _ -> Stop depth
        | None -> Continue (depth + 1))
      ~finish:(fun depth -> depth)
      scopes
  in
  match Map.add resolution ~key:id ~data:depth with
  | `Duplicate ->
      Result.fail
        [
          Printf.sprintf
            "The AST node already exists in the resultion map: `%d`" id;
        ]
  | _ -> Ok resolution

let rec resolve_function (resolution : resolution) (scopes : scopes) = function
  | Function (_, args, stmts, _) ->
      Stack.push scopes (new_scope ());
      let%bind _ =
        List.fold ~init:Result.ok_unit
          ~f:(fun acc arg ->
            let%bind _ = acc in
            match arg with
            | { kind = Identifier n; _ } ->
                let%bind _ = declare_var scopes n in
                define_var scopes n
            | { kind; _ } ->
                Result.fail
                  [
                    Printf.sprintf "Invalid function argument: %s "
                      (kind |> Lex.sexp_of_token_kind |> Sexp.to_string_hum);
                  ])
          args
      in
      let%bind resolution = resolve_stmts resolution scopes stmts in
      Stack.pop_exn scopes |> ignore;
      Ok resolution
  | _ as f ->
      Result.fail
        [
          Printf.sprintf "Invalid function declaration: %s "
            (f |> sexp_of_statement |> Sexp.to_string_hum);
        ]

and resolve_expr (resolution : resolution) (scopes : scopes) = function
  | Assign (Lex.Identifier n, expr, id) ->
      let%bind resolution = resolve_expr resolution scopes expr in
      resolve_local resolution scopes id n
  | Variable (Lex.Identifier n, id) ->
      let%bind scope =
        Stack.top scopes |> Result.of_option ~error:[ "Missing scope" ]
      in

      let%bind initialized =
        Hashtbl.find scope n |> Result.of_option ~error:[ "Missing variable" ]
      in
      if Bool.equal initialized false then
        Result.fail
          [
            Printf.sprintf "Cannot read variable `%s` in its own initializer" n;
          ]
      else resolve_local resolution scopes id n
  | Call (callee, _, args, _) ->
      let resolution = resolve_expr resolution scopes callee in
      List.fold ~init:resolution
        ~f:(fun resolution arg ->
          let%bind resolution = resolution in
          resolve_expr resolution scopes arg)
        args
  | Binary (left, _, right, _)
  | LogicalOr (left, right, _)
  | LogicalAnd (left, right, _) ->
      let%bind resolution = resolve_expr resolution scopes left in
      resolve_expr resolution scopes right
  | Unary (_, e, _) | Grouping (e, _) -> resolve_expr resolution scopes e
  | Literal _ -> Ok resolution
  | Assign _ as a ->
      Result.fail
        [
          Printf.sprintf "Invalid assignment: %s "
            (a |> sexp_of_expr |> Sexp.to_string_hum);
        ]
  | Variable _ as v ->
      Result.fail
        [
          Printf.sprintf "Invalid variable: %s "
            (v |> sexp_of_expr |> Sexp.to_string_hum);
        ]

and resolve_stmt (resolution : resolution) (scopes : scopes) = function
  | Block (stmts, _) ->
      Stack.push scopes (new_scope ());
      let%bind resolution =
        Array.fold
          ~f:(fun resolution stmt ->
            let%bind resolution = resolution in
            resolve_stmt resolution scopes stmt)
          ~init:(Ok resolution) stmts
      in
      Stack.pop_exn scopes |> ignore;
      Ok resolution
  | Var (Lex.Identifier n, expr, _) ->
      let%bind _ = declare_var scopes n in
      let%bind resolution = resolve_expr resolution scopes expr in
      let%bind _ = define_var scopes n in
      Ok resolution
  | Print (e, _) | Expr (e, _) | Return (_, e, _) ->
      resolve_expr resolution scopes e
  | Function ({ Lex.kind = Lex.Identifier name; _ }, _, _, _) as fn ->
      let%bind _ = declare_var scopes name in
      let%bind _ = define_var scopes name in
      resolve_function resolution scopes fn
  | WhileStmt (e, stmt, _) | IfStmt (e, stmt, _) ->
      let%bind resolution = resolve_expr resolution scopes e in
      resolve_stmt resolution scopes stmt
  | IfElseStmt (e, then_stmt, else_stmt, _) ->
      let%bind resolution = resolve_expr resolution scopes e in
      let%bind resolution = resolve_stmt resolution scopes then_stmt in
      resolve_stmt resolution scopes else_stmt
  | Function _ as f ->
      Result.fail
        [
          Printf.sprintf "Invalid function declaration: %s "
            (f |> sexp_of_statement |> Sexp.to_string_hum);
        ]
  | Var _ as v ->
      Result.fail
        [
          Printf.sprintf "Invalid variable declaration: %s "
            (v |> sexp_of_statement |> Sexp.to_string_hum);
        ]

and resolve_stmts (resolution : resolution) (scopes : scopes)
    (stmts : statement list) =
  List.fold
    ~f:(fun resolution stmt ->
      let%bind resolution = resolution in
      resolve_stmt resolution scopes stmt)
    ~init:(Ok resolution) stmts

let resolve (stmts : statement list) =
  let resolution : resolution = Map.empty (module Int) in
  let scopes : scopes = Stack.create () in
  Stack.push scopes (new_scope ());
  let%bind resolution = resolve_stmts resolution scopes stmts in
  Stack.pop_exn scopes |> ignore;
  Ok (stmts, resolution)
