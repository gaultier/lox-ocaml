open Parse

type t = (string, value, Base.String.comparator_witness) Base.Map.t

let empty = Base.Map.empty (module Base.String)

type environment = {mutable values: t; enclosing: environment option}

let rec find_in_environment env v =
  match Base.Map.find env.values v with
  | Some v ->
      v
  | None -> (
    match env.enclosing with
    | Some e ->
        find_in_environment e v
    | None ->
        failwith (Printf.sprintf "Accessing unbound variable %s" v) )

let rec assign_in_environment env n v =
  match Base.Map.find env.values n with
  | Some v ->
      env.values <- Base.Map.set ~key:n ~data:v env.values
  | None -> (
    match env.enclosing with
    | Some e ->
        assign_in_environment e n v
    | None ->
        failwith (Printf.sprintf "Accessing unbound variable %s" n) )

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
            failwith
              (Printf.sprintf "Unary expression not allowed: %s %s"
                 (Lex.token_to_string t) (value_to_string v))
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
      let v = find_in_environment env n in
      (v, env)
  | Variable _ ->
      failwith "Badly constructed var"
  | Assign (Lex.Identifier n, e) ->
      let e, env = eval_exp e env in
      assign_in_environment env n e ;
      Printf.printf "[D001] %s=%s\n" n
        ( Base.Map.find env.values n |> Option.map value_to_string
        |> Base.Option.value ~default:"?" ) ;
      (e, env)
  | Assign (t, _) ->
      failwith
        (Printf.sprintf "Invalid assignment: %s " (Lex.token_to_string t))
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
          failwith
            (Printf.sprintf "Division by zero not allowed: %s %s %s"
               (value_to_string l) (Lex.token_to_string t) (value_to_string r))
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
          failwith ("Binary expression not allowed: " ^ Lex.token_to_string t)
      )

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
      failwith
        (Printf.sprintf "Invalid variable declaration: %s"
           (Lex.token_to_string t))
  | Block stmts ->
      let enclosed_env = {values= empty; enclosing= Some env} in
      let _ =
        Array.fold_left
          (fun enclosed_env s ->
            let _, enclosed_env = eval s enclosed_env in
            enclosed_env)
          enclosed_env stmts
      in
      (Nil, env)
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
