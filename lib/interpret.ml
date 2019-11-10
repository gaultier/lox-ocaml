open Parse
open Base

exception FunctionReturn of value * environment

let rec find_in_environment n = function
  | [] ->
      Printf.failwithf "Accessing unbound variable `%s`" n ()
  | x :: xs -> (
    match Map.find x n with Some v -> v | None -> find_in_environment n xs )

let assign_in_environment n v env =
  let rec assign_rec acc = function
    | [] ->
        Printf.failwithf "Assigning unbound variable `%s` to `%s`" n
          (value_to_string v) ()
    | x :: xs -> (
      match Map.find x n with
      | Some _ ->
          let x = Map.set ~key:n ~data:v x in
          xs @ (x :: acc)
      | None ->
          assign_rec (x :: acc) xs )
  in
  assign_rec [] env |> List.rev

let create_in_current_env n v = function
  | [] ->
      failwith "Empty environment, should never happen"
  | x :: xs ->
      let x = Map.set ~key:n ~data:v x in
      x :: xs

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
            Printf.failwithf "Unary expression not allowed: %s %s"
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
      let v = find_in_environment n env in
      (v, env)
  | Variable _ ->
      failwith "Badly constructed var"
  | Assign (Lex.Identifier n, e) ->
      let v, env = eval_exp e env in
      let env = assign_in_environment n v env in
      (v, env)
  | Assign (t, _) ->
      Printf.failwithf "Invalid assignment: %s " (Lex.token_to_string t) ()
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
          Printf.failwithf "Division by zero not allowed: %s %s %s"
            (value_to_string l) (Lex.token_to_string t) (value_to_string r) ()
      | Number a, Lex.Slash, Number b ->
          (Number (a /. b), env)
      | Number a, Lex.Star, Number b ->
          (Number (a *. b), env)
      | Number a, Lex.Less, Number b ->
          (Bool (Float.( < ) a b), env)
      | Number a, Lex.LessEqual, Number b ->
          (Bool (Float.( <= ) a b), env)
      | Number a, Lex.Greater, Number b ->
          (Bool (Float.( > ) a b), env)
      | Number a, Lex.GreaterEqual, Number b ->
          (Bool (Float.( >= ) a b), env)
      | Number a, Lex.BangEqual, Number b ->
          (Bool (not (Float.equal a b)), env)
      | Number a, Lex.EqualEqual, Number b ->
          (Bool (Float.equal a b), env)
      | String a, Lex.EqualEqual, String b ->
          (Bool (String.equal a b), env)
      | Bool a, Lex.EqualEqual, Bool b ->
          (Bool (Bool.equal a b), env)
      | Nil, Lex.EqualEqual, Nil ->
          (Bool true, env)
      | Nil, Lex.EqualEqual, _ ->
          (Bool false, env)
      | _, Lex.EqualEqual, Nil ->
          (Bool false, env)
      | _ ->
          Printf.failwithf "Binary expression not allowed: %s"
            (Lex.token_to_string t) () )
  | Call (callee, _, args) ->
      let e, env = eval_exp callee env in
      let f =
        match e with
        | Callable f ->
            f
        | _ ->
            Printf.failwithf "Value `%s` cannot be called as a function"
              (value_to_string e) ()
      in
      let args, env =
        List.fold ~init:([], env)
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
            Printf.failwithf
              "Wrong arity in function call: expected %d, got %d" f.arity len
              ()
      in
      let v, _ = f.fn args f.decl_environment in
      (v, env)

let rec eval s env =
  match s with
  | Expr e ->
      (eval_exp [@tailcall]) e env
  | Print e ->
      let v, env = eval_exp e env in
      v |> Parse.value_to_string |> Stdlib.print_endline ;
      (Nil, env)
  | Var (Lex.Identifier n, e) ->
      let e, env = eval_exp e env in
      let env = create_in_current_env n e env in
      (e, env)
  | Var (t, _) ->
      Printf.failwithf "Invalid variable declaration: %s"
        (Lex.token_to_string t) ()
  | Block stmts ->
      let env = empty :: env in
      let _ =
        Array.fold
          ~f:(fun env s ->
            let _, env = eval s env in
            env)
          ~init:env stmts
      in
      (Nil, List.tl_exn env)
  | Return (_, expr) ->
      let v, env = eval_exp expr env in
      raise (FunctionReturn (v, env))
  | Function ({Lex.kind= Lex.Identifier name; _}, decl_args, body) ->
      let decl_env = env in
      let fn call_args env =
        let env =
          List.fold2_exn
            ~f:(fun env decl_arg call_arg ->
              match decl_arg with
              | {Lex.kind= Identifier n; _} ->
                  create_in_current_env n call_arg env
              | _ ->
                  failwith "Invalid function argument")
            ~init:(empty :: env) decl_args call_args
        in
        let v, env =
          List.fold ~init:(None, env)
            ~f:(fun (v, env) stmt ->
              match v with
              | Some _ as ret_value ->
                  (ret_value, env)
              | None -> (
                try
                  let _, env = eval stmt env in
                  (None, env)
                with FunctionReturn (v, env) -> (Some v, env) ))
            body
        in
        let v = Option.value v ~default:Nil in
        (v, List.tl_exn env)
      in
      let call =
        {arity= List.length decl_args; name; decl_environment= decl_env; fn}
      in
      let decl_env = create_in_current_env name (Callable call) decl_env in
      call.decl_environment <- decl_env ;
      (Nil, decl_env)
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
    stmts |> List.to_array
    |> Array.fold
         ~init:([||], env)
         ~f:(fun (acc, env) s ->
           let e, env = eval s env in
           (Array.append acc [|e|], env))
    |> fun (stmts, env) -> Ok (stmts, env)
  with Failure err -> Result.Error [err]
