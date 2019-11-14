open Parse
open Base

exception FunctionReturn of value

let print_env env =
  let rec print_env_rec {values; enclosing} =
    Hashtbl.iteri
      ~f:(fun ~key:k ~data:v ->
        Stdlib.Printf.printf "%s=%s " k (value_to_string v))
      values ;
    Option.iter ~f:print_env_rec enclosing
  in
  Stdlib.print_string "[ " ; print_env_rec env ; Stdlib.print_string "]\n"

let rec find_in_environment n {values; enclosing} =
  match (Hashtbl.find values n, enclosing) with
  | Some v, _ ->
      v
  | None, Some enclosing ->
      find_in_environment n enclosing
  | None, None ->
      Printf.failwithf "Accessing unbound variable `%s`" n ()

let rec assign_in_environment n v {values; enclosing} =
  match (Hashtbl.find values n, enclosing) with
  | Some _, _ ->
      Hashtbl.set ~key:n ~data:v values
  | None, Some enclosing ->
      assign_in_environment n v enclosing
  | None, None ->
      Printf.failwithf "Assigning unbound variable `%s` to `%s`" n
        (value_to_string v) ()

let create_in_current_env n v {values; _} = Hashtbl.set ~key:n ~data:v values

let rec eval_exp exp (env : environment) =
  match exp with
  | Grouping e ->
      (eval_exp [@tailcall]) e env
  | Unary (t, e) ->
      let v = eval_exp e env in
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
      res
  | Literal l ->
      l
  | LogicalOr (l, r) -> (
      let e = eval_exp l env in
      match e with Bool false | Nil -> (eval_exp [@tailcall]) r env | _ -> e )
  | LogicalAnd (l, r) -> (
      let e = eval_exp l env in
      match e with Bool false | Nil -> e | _ -> (eval_exp [@tailcall]) r env )
  | Variable (Lex.Identifier n) ->
      let v = find_in_environment n env in
      v
  | Variable _ ->
      failwith "Badly constructed var"
  | Assign (Lex.Identifier n, e) ->
      let v = eval_exp e env in
      assign_in_environment n v env ;
      v
  | Assign (t, _) ->
      Printf.failwithf "Invalid assignment: %s " (Lex.token_to_string t) ()
  | Binary (l, t, r) -> (
      let l = eval_exp l env in
      let r = eval_exp r env in
      match (l, t, r) with
      | String a, Lex.Plus, String b ->
          String (a ^ b)
      | Number a, Lex.Plus, Number b ->
          Number (a +. b)
      | Number a, Lex.Minus, Number b ->
          Number (a -. b)
      | Number _, Lex.Slash, Number 0. ->
          Printf.failwithf "Division by zero not allowed: %s %s %s"
            (value_to_string l) (Lex.token_to_string t) (value_to_string r) ()
      | Number a, Lex.Slash, Number b ->
          Number (a /. b)
      | Number a, Lex.Star, Number b ->
          Number (a *. b)
      | Number a, Lex.Less, Number b ->
          Bool (Float.( < ) a b)
      | Number a, Lex.LessEqual, Number b ->
          Bool (Float.( <= ) a b)
      | Number a, Lex.Greater, Number b ->
          Bool (Float.( > ) a b)
      | Number a, Lex.GreaterEqual, Number b ->
          Bool (Float.( >= ) a b)
      | Number a, Lex.BangEqual, Number b ->
          Bool (not (Float.equal a b))
      | Number a, Lex.EqualEqual, Number b ->
          Bool (Float.equal a b)
      | String a, Lex.EqualEqual, String b ->
          Bool (String.equal a b)
      | Bool a, Lex.EqualEqual, Bool b ->
          Bool (Bool.equal a b)
      | Nil, Lex.EqualEqual, Nil ->
          Bool true
      | Nil, Lex.EqualEqual, _ ->
          Bool false
      | _, Lex.EqualEqual, Nil ->
          Bool false
      | _ ->
          Printf.failwithf "Binary expression not allowed: %s"
            (Lex.token_to_string t) () )
  | Call (callee, _, args) ->
      let e = eval_exp callee env in
      let f =
        match e with
        | Callable f ->
            f
        | _ ->
            Printf.failwithf "Value `%s` cannot be called as a function"
              (value_to_string e) ()
      in
      let args =
        List.fold ~init:[]
          ~f:(fun values a ->
            let v = eval_exp a env in
            v :: values)
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
      let v = f.fn args f.decl_environment in
      v

let rec eval s (env : environment) =
  match s with
  | Expr e ->
      (eval_exp [@tailcall]) e env
  | Print e ->
      let v = eval_exp e env in
      v |> Parse.value_to_string |> Stdlib.print_endline ;
      Nil
  | Var (Lex.Identifier n, e) ->
      let e = eval_exp e env in
      create_in_current_env n e env ;
      e
  | Var (t, _) ->
      Printf.failwithf "Invalid variable declaration: %s"
        (Lex.token_to_string t) ()
  | Block stmts ->
      let enclosing = env in
      let env = {values= empty (); enclosing= Some enclosing} in
      Array.iter ~f:(fun s -> eval s env |> ignore) stmts ;
      Nil
  | Return (_, expr) ->
      let v = eval_exp expr env in
      raise (FunctionReturn v)
  | Function ({Lex.kind= Lex.Identifier name; _}, decl_args, body) ->
      let fn call_args (env : environment) =
        let enclosing = env in
        let env = {values= empty (); enclosing= Some enclosing} in
        List.iter2_exn
          ~f:(fun n v ->
            match n with
            | {Lex.kind= Identifier n; _} ->
                create_in_current_env n v env
            | _ ->
                failwith "Invalid function argument")
          decl_args call_args ;
        let v =
          List.fold ~init:None
            ~f:(fun v stmt ->
              match v with
              | Some _ as ret_value ->
                  ret_value
              | None -> (
                try
                  let _ = eval stmt env in
                  None
                with FunctionReturn v -> Some v ))
            body
        in
        let v = Option.value v ~default:Nil in
        v
      in
      let call =
        {arity= List.length decl_args; name; decl_environment= env; fn}
      in
      create_in_current_env name (Callable call) env ;
      call.decl_environment <- env ;
      Nil
  | Function _ ->
      failwith "Invalid function declaration"
  | IfElseStmt (e, then_stmt, else_stmt) -> (
      let e = eval_exp e env in
      match e with
      | Bool false | Nil ->
          (eval [@tailcall]) else_stmt env
      | _ ->
          (eval [@tailcall]) then_stmt env )
  | IfStmt (e, then_stmt) -> (
      let e = eval_exp e env in
      match e with
      | Bool false | Nil ->
          Nil
      | _ ->
          (eval [@tailcall]) then_stmt env )
  | WhileStmt _ ->
      (eval_while [@tailcall]) s env

and eval_while w env =
  match w with
  | WhileStmt (e, s) -> (
      let e = eval_exp e env in
      match e with
      | Bool false | Nil ->
          Nil
      | _ ->
          let _ = eval s env in
          (eval_while [@tailcall]) w env )
  | _ ->
      failwith "Invalid while statement"

let interpret (env : environment) stmts =
  try
    stmts |> List.to_array
    |> Array.fold ~init:[||] ~f:(fun acc s ->
           let e = eval s env in
           Array.append acc [|e|])
    |> fun stmts -> Ok stmts
  with Failure err -> Result.Error [err]
