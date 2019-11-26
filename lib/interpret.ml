open Parse
open Base

exception FunctionReturn of value

let print_env_values values = Hashtbl.iteri ~f:(fun ~key:k ~data:d -> Stdlib.Printf.printf "- %s = %s\n" k (value_to_string d )) values

let rec climb_nth_env depth = function
    | {enclosing=Some enclosing; _} when depth > 0 -> climb_nth_env (depth-1) enclosing
    | {enclosing=None; _} when depth > 0 -> Printf.failwithf "Failed assertion: mismatch between var resolution & interpreter when finding variable: depth=%d but there are no more environments to search upwards" depth ()
    | env when depth = 0 -> env
    | _ -> failwith "Unreachable climb_nth_env"

let find_in_environment (expr: expr) (var_resolution: Var_resolver.resolution) env =
              Stdlib.Printf.printf "Find in env expr=%s\n" (expr |> sexp_of_expr |> Sexp.to_string_hum);
    let depth: int = Map.find var_resolution expr |> Stdlib.Option.get  in
              Stdlib.Printf.printf "Find in env depth=%d\n" depth;
    let env = climb_nth_env depth env in 
              Stdlib.Printf.printf "Find in env: finished climbing \n" ;
     let n = match expr with  | Variable (Lex.Identifier n) -> n | _ -> failwith "Malformed variable" in 
     let v =  match  Hashtbl.find env.values n with
  |  Some v -> v
  |  None -> print_env_values env.values; Printf.failwithf "Accessing unbound variable `%s`." n ()
     in 
              Stdlib.Printf.printf "Find in env: v=%s\n" (value_to_string v);
              v

let assign_in_environment (expr: expr) v (var_resolution: Var_resolver.resolution) env =
    let depth: int = Map.find var_resolution expr |> Stdlib.Option.get 
     in 
    let env = climb_nth_env depth env in 
       let n = match expr with         | Assign (Lex.Identifier n, _) -> n | _ -> failwith "Malformed variable" in 
  match Hashtbl.find env.values n with
  | Some _ -> Hashtbl.set ~key:n ~data:v env.values
  | None -> Printf.failwithf "Assigning unbound variable `%s` to `%s`" n
        (value_to_string v) ()

let create_in_current_env n v { values; _ } = Hashtbl.set ~key:n ~data:v values

let rec eval_exp exp (var_resolution: Var_resolver.resolution) ( env : environment) =
  match exp with
  | Grouping e -> eval_exp e var_resolution env
  | Unary (t, e) ->
      let v = eval_exp e var_resolution env in
      let res =
        match (t, v) with
        | Lex.Minus, Number f -> Number (-.f)
        | Lex.Bang, Nil | Lex.Bang, Bool false -> Bool true
        | Lex.Bang, _ -> Bool false
        | _ ->
            Printf.failwithf "Unary expression not allowed: %s %s"
              (Lex.token_to_string t) (value_to_string v) ()
      in
      res
  | Literal l -> l
  | LogicalOr (l, r) -> (
      let e = eval_exp l var_resolution env in
      match e with Bool false | Nil -> eval_exp r var_resolution env | _ -> e )
  | LogicalAnd (l, r) -> (
      let e = eval_exp l var_resolution env in
      match e with Bool false | Nil -> e | _ -> eval_exp r var_resolution env )
  | Variable (Lex.Identifier _) as var  -> find_in_environment var var_resolution env
  | Variable _ -> failwith "Badly constructed var"
  | Assign (Lex.Identifier _, e) as expr ->
      let v = eval_exp e var_resolution env in
      assign_in_environment expr v var_resolution env;
      v
  | Assign (t, _) ->
      Printf.failwithf "Invalid assignment: %s " (Lex.token_to_string t) ()
  | Binary (l, t, r) -> (
      let l = eval_exp l var_resolution env in
      let r = eval_exp r var_resolution env in
      match (l, t, r) with
      | String a, Lex.Plus, String b -> String (a ^ b)
      | Number a, Lex.Plus, Number b -> Number (a +. b)
      | Number a, Lex.Minus, Number b -> Number (a -. b)
      | Number _, Lex.Slash, Number 0. ->
          Printf.failwithf "Division by zero not allowed: %s %s %s"
            (value_to_string l) (Lex.token_to_string t) (value_to_string r) ()
      | Number a, Lex.Slash, Number b -> Number (a /. b)
      | Number a, Lex.Star, Number b -> Number (a *. b)
      | Number a, Lex.Less, Number b -> Bool (Float.( < ) a b)
      | Number a, Lex.LessEqual, Number b -> Bool (Float.( <= ) a b)
      | Number a, Lex.Greater, Number b -> Bool (Float.( > ) a b)
      | Number a, Lex.GreaterEqual, Number b -> Bool (Float.( >= ) a b)
      | Number a, Lex.BangEqual, Number b -> Bool (not (Float.equal a b))
      | Number a, Lex.EqualEqual, Number b -> Bool (Float.equal a b)
      | String a, Lex.EqualEqual, String b -> Bool (String.equal a b)
      | Bool a, Lex.EqualEqual, Bool b -> Bool (Bool.equal a b)
      | Nil, Lex.EqualEqual, Nil -> Bool true
      | Nil, Lex.EqualEqual, _ -> Bool false
      | _, Lex.EqualEqual, Nil -> Bool false
      | _ ->
          Printf.failwithf "Binary expression not allowed: %s"
            (Lex.token_to_string t) () )
  | Call (callee, _, args) ->
      let e = eval_exp callee var_resolution env in
      let f =
        match e with
        | Callable f -> f
        | _ ->
            Printf.failwithf "Value `%s` cannot be called as a function"
              (value_to_string e) ()
      in
      let args = List.map ~f:(fun a -> eval_exp a var_resolution env) args in
      let len = List.length args in
      let _ =
        match len with
        | l when l = f.arity -> l
        | _ ->
            Printf.failwithf "Wrong arity in function call: expected %d, got %d"
              f.arity len ()
      in
      f.fn args f.decl_environment

let rec eval s (var_resolution: Var_resolver.resolution) (env : environment) =
  match s with
  | Expr e -> eval_exp e var_resolution env
  | Print e ->
              Stdlib.Printf.printf "Print stmt of e=%s\n" (e |> sexp_of_expr |> Sexp.to_string_hum);
      let v = eval_exp e var_resolution env in
              Stdlib.Printf.printf "Print stmt of v=%s\n" (v |> value_to_string);
      v |> Parse.value_to_string |> Stdlib.print_endline;
      Nil
  | Var (Lex.Identifier n, e) ->
      let e = eval_exp e var_resolution env in
      create_in_current_env n e env;
      e
  | Var (t, _) ->
      Printf.failwithf "Invalid variable declaration: %s"
        (Lex.token_to_string t) ()
  | Block stmts ->
      let enclosing = env in
      let env = { values = empty (); enclosing = Some enclosing } in
      Array.iter ~f:(fun s -> eval s var_resolution env |> ignore) stmts;
      Nil
  | Return (_, expr) ->
      let v = eval_exp expr var_resolution env in
      raise (FunctionReturn v)
  | Function ({ Lex.kind = Lex.Identifier name; _ }, decl_args, body) ->
      let fn call_args (env : environment) =
        let enclosing = env in
        let env = { values = empty (); enclosing = Some enclosing } in
        List.iter2_exn
          ~f:(fun n v ->
            match n with
            | { Lex.kind = Identifier n; _ } -> create_in_current_env n v env
            | _ -> failwith "Invalid function argument")
          decl_args call_args;
        try
          List.iter ~f:(fun stmt -> eval stmt var_resolution env |> ignore) body;
          Nil
        with FunctionReturn v -> v
      in
      let call =
        { arity = List.length decl_args; name; decl_environment = env; fn }
      in
      create_in_current_env name (Callable call) env;
      call.decl_environment <- env;
      Nil
  | Function _ -> failwith "Invalid function declaration"
  | IfElseStmt (e, then_stmt, else_stmt) -> (
      let e = eval_exp e var_resolution env in
      match e with
      | Bool false | Nil -> eval else_stmt var_resolution env
      | _ -> eval then_stmt var_resolution env )
  | IfStmt (e, then_stmt) -> (
      let e = eval_exp e var_resolution env in
      match e with Bool false | Nil -> Nil | _ -> eval then_stmt var_resolution env )
  | WhileStmt _ -> eval_while s var_resolution env

and eval_while w var_resolution env =
  match w with
  | WhileStmt (e, s) -> (
      let e = eval_exp e var_resolution env in
      match e with
      | Bool false | Nil -> Nil
      | _ ->
          eval s var_resolution env |> ignore;
          eval_while w var_resolution env )
  | _ -> failwith "Invalid while statement"

let interpret (var_resolution: Var_resolver.resolution) (env : environment) stmts =
  try
    stmts |> List.to_array |> Array.map ~f:(fun s -> eval s var_resolution env) |> fun stmts ->
    Ok stmts
  with Failure err -> Result.Error [ err ]
