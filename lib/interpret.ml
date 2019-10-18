open Parse
module StringMap = Map.Make (String)

let rec eval_exp exp env =
  match exp with
  | Grouping e ->
      eval_exp e env
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
            failwith ("Unary expression not allowed: " ^ "*")
      in
      (res, env)
  | Literal l ->
      (l, env)
  | LogicalOr (l, r) -> (
      let e, env = eval_exp l env in
      match e with Bool false | Nil -> eval_exp r env | _ -> (e, env) )
  | LogicalAnd (l, r) -> (
      let e, env = eval_exp l env in
      match e with Bool false | Nil -> (e, env) | _ -> eval_exp r env )
  | Variable (Lex.Identifier n) -> (
    match StringMap.find_opt n env with
    | Some v ->
        (v, env)
    | None ->
        failwith ("Accessing unkown variable `" ^ n ^ "`") )
  | Variable _ ->
      failwith "Badly constructed var"
  | Assign (Lex.Identifier n, e) ->
      let e, env = eval_exp e env in
      let env = StringMap.add n e env in
      (e, env)
  | Assign _ ->
      failwith "Badly constructed assignement"
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
          failwith ("Division by zero not allowed: " ^ "*")
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
          failwith ("Binary expression not allowed: " ^ "*") )

let rec eval s env =
  match s with
  | Expr e ->
      eval_exp e env
  | Print e ->
      let v, env = eval_exp e env in
      v |> Parse.value_to_string |> print_endline ;
      (Nil, env)
  | Var (Lex.Identifier n, e) ->
      let e, env = eval_exp e env in
      let env = StringMap.add n e env in
      (e, env)
  | Var _ ->
      failwith "Badly constructed var"
  | Block stmts ->
      ( Nil
      , Array.fold_left
          (fun env s ->
            let _, env = eval s env in
            env)
          env stmts )
  | IfElseStmt (e, then_stmt, else_stmt) -> (
      let e, env = eval_exp e env in
      match e with
      | Bool false | Nil ->
          eval else_stmt env
      | _ ->
          eval then_stmt env )
  | IfStmt (e, then_stmt) -> (
      let e, env = eval_exp e env in
      match e with Bool false | Nil -> (Nil, env) | _ -> eval then_stmt env )
  | WhileStmt _ ->
      eval_while s env

and eval_while w env =
  match w with
  | WhileStmt (e, s) -> (
      let e, env = eval_exp e env in
      match e with
      | Bool false | Nil ->
          (Nil, env)
      | _ ->
          let _, env = eval s env in
          eval_while w env )
  | _ ->
      failwith "Wrong call"

let interpret env stmts =
  let arr_stmts = Base.List.to_array stmts in
  Ok
    (Base.Array.fold arr_stmts
       ~init:([||], env)
       ~f:(fun (acc, env) s ->
         let e, env = eval s env in
         (Array.append acc [|e|], env)))
