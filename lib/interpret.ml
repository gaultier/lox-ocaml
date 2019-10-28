open Parse

let rec find_in_environment env v =
  match Base.Map.find env.values v with
  | Some v ->
      v
  | None -> (
    match env.enclosing with
    | Some e ->
        find_in_environment e v
    | None ->
        Base.Printf.failwithf "Accessing unbound variable %s" v () )

let rec assign_in_environment env n v =
  match Base.Map.find env.values n with
  | Some _ ->
      env.values <- Base.Map.set ~key:n ~data:v env.values
  | None -> (
    match env.enclosing with
    | Some env ->
        assign_in_environment env n v
    | None ->
        Base.Printf.failwithf "Accessing unbound variable %s" n () )

let rec assign_in_global_environment n v = function
  | {enclosing= None; _} as env ->
      env.values <- Base.Map.set ~key:n ~data:v env.values
  | {enclosing= Some enc; _} ->
      (assign_in_global_environment [@tailcall]) n v enc

let rec eval_exp exp env =
  match exp with
  | Grouping e ->
      (eval_exp [@tailcall]) e env
  | Unary (t, e) ->
      let v, env = eval_exp e env in
      let res =
        match (t, v) with
        | Lex.Minus, Number f ->
            Number (-.f)
        | Lex.Bang, Nil | Lex.Bang, Bool false ->
            Bool true
        | Lex.Bang, _ ->
            Bool false
        | _ ->
            Base.Printf.failwithf "Unary expression not allowed: %s %s"
              (Lex.token_to_string t) (value_to_string v) ()
      in
      (res, env)
  | Literal l ->
      (l, env)
  | LogicalOr (l, r) -> (
      let e, env = eval_exp l env in
      match e with
      | Bool false | Nil ->
          (eval_exp [@tailcall]) r env
      | _ ->
          (e, env) )
  | LogicalAnd (l, r) -> (
      let e, env = eval_exp l env in
      match e with
      | Bool false | Nil ->
          (e, env)
      | _ ->
          (eval_exp [@tailcall]) r env )
  | Variable (Lex.Identifier n) ->
      (find_in_environment env n, env)
  | Variable _ ->
      failwith "Badly constructed var"
  | Assign (Lex.Identifier n, e) ->
      let e, env = eval_exp e env in
      assign_in_environment env n e ;
      (e, env)
  | Assign (t, _) ->
      Base.Printf.failwithf "Invalid assignment: %s " (Lex.token_to_string t)
        ()
  | Binary (l, t, r) -> (
      let l, env = eval_exp l env in
      let r, env = eval_exp r env in
      match (l, t, r) with
      | String a, Lex.Plus, String b ->
          (String (a ^ b), env)
      | Number a, Lex.Plus, Number b ->
          (Number (a +. b), env)
      | Number a, Lex.Minus, Number b ->
          (Number (a -. b), env)
      | Number _, Lex.Slash, Number 0. ->
          Base.Printf.failwithf "Division by zero not allowed: %s %s %s"
            (value_to_string l) (Lex.token_to_string t) (value_to_string r) ()
      | Number a, Lex.Slash, Number b ->
          (Number (a /. b), env)
      | Number a, Lex.Star, Number b ->
          (Number (a *. b), env)
      | Number a, Lex.Less, Number b ->
          (Bool (a < b), env)
      | Number a, Lex.LessEqual, Number b ->
          (Bool (a <= b), env)
      | Number a, Lex.Greater, Number b ->
          (Bool (a > b), env)
      | Number a, Lex.GreaterEqual, Number b ->
          (Bool (a >= b), env)
      | Number a, Lex.BangEqual, Number b ->
          (Bool (a != b), env)
      | Number a, Lex.EqualEqual, Number b ->
          (Bool (Float.equal a b), env)
      | String a, Lex.EqualEqual, String b ->
          (Bool (String.equal a b), env)
      | Bool a, Lex.EqualEqual, Bool b ->
          (Bool (a == b), env)
      | Nil, Lex.EqualEqual, Nil ->
          (Bool true, env)
      | Nil, Lex.EqualEqual, _ ->
          (Bool false, env)
      | _, Lex.EqualEqual, Nil ->
          (Bool false, env)
      | _ ->
          Base.Printf.failwithf "Binary expression not allowed: %s"
            (Lex.token_to_string t) () )
  | Call (callee, _, args) ->
      let e, env = eval_exp callee env in
      let f =
        match e with
        | Callable f ->
            f
        | _ ->
            Base.Printf.failwithf "Value `%s` cannot be called as a function"
              (value_to_string e) ()
      in
      let args, env =
        Base.List.fold ~init:([], env)
          ~f:(fun acc a ->
            let values, env = acc in
            let v, env = eval_exp a env in
            (v :: values, env))
          args
      in
      let len = List.length args in
      let _ =
        match len with
        | l when l = f.arity ->
            l
        | _ ->
            Base.Printf.failwithf
              "Wrong arity in function call: expected %d, got %d" f.arity len
              ()
      in
      let v, env = f.fn args env in
      (v, env)

let rec eval s env =
  match s with
  | Expr e ->
      (eval_exp [@tailcall]) e env
  | Print e ->
      let v, env = eval_exp e env in
      v |> Parse.value_to_string |> print_endline ;
      (Nil, env)
  | Var (Lex.Identifier n, e) ->
      let e, env = eval_exp e env in
      let env = {env with values= Base.Map.set ~key:n ~data:e env.values} in
      (e, env)
  | Var (t, _) ->
      Base.Printf.failwithf "Invalid variable declaration: %s"
        (Lex.token_to_string t) ()
  | Block stmts ->
      let enclosed_env = {values= globals; enclosing= Some env} in
      let _ =
        Array.fold_left
          (fun enclosed_env s ->
            let _, enclosed_env = eval s enclosed_env in
            enclosed_env)
          enclosed_env stmts
      in
      (Nil, env)
  | Function ({Lex.kind= Lex.Identifier name; _}, decl_args, body) ->
      let fn =
        Callable
          { arity= List.length decl_args
          ; name
          ; fn=
              (fun call_args env ->
                List.iter2
                  (fun decl_arg call_arg ->
                    match decl_arg with
                    | {Lex.kind= Identifier n; _} ->
                        assign_in_environment env n call_arg
                    | _ ->
                        failwith "Invalid function argument")
                  decl_args call_args ;
                let env =
                  Base.List.fold ~init:env
                    ~f:(fun env stmt ->
                      let _, env = eval stmt env in
                      env)
                    body
                in
                (Nil, env)) }
      in
      assign_in_global_environment name fn env ;
      (Nil, env)
  | Function _ ->
      failwith "Invalid function declaration"
  | IfElseStmt (e, then_stmt, else_stmt) -> (
      let e, env = eval_exp e env in
      match e with
      | Bool false | Nil ->
          (eval [@tailcall]) else_stmt env
      | _ ->
          (eval [@tailcall]) then_stmt env )
  | IfStmt (e, then_stmt) -> (
      let e, env = eval_exp e env in
      match e with
      | Bool false | Nil ->
          (Nil, env)
      | _ ->
          (eval [@tailcall]) then_stmt env )
  | WhileStmt _ ->
      (eval_while [@tailcall]) s env

and eval_while w env =
  match w with
  | WhileStmt (e, s) -> (
      let e, env = eval_exp e env in
      match e with
      | Bool false | Nil ->
          (Nil, env)
      | _ ->
          let _, env = eval s env in
          (eval_while [@tailcall]) w env )
  | _ ->
      failwith "Invalid while statement"

let interpret env stmts =
  try
    stmts |> Base.List.to_array
    |> Base.Array.fold
         ~init:([||], env)
         ~f:(fun (acc, env) s ->
           let e, env = eval s env in
           (Array.append acc [|e|], env))
    |> Result.ok
  with Failure err -> Result.Error [err]
