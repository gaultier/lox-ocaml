open Parse
open Base
open Base.Option.Let_syntax
open Monad_utils

exception FunctionReturn of value

let print_env_values values =
  Hashtbl.iteri
    ~f:(fun ~key:k ~data:d ->
      Stdlib.Printf.printf "- %s = %s\n" k (value_to_string d))
    values

let rec climb_nth_env env depth =
  match (env, depth) with
  | env, 0 -> Some env
  | env, depth ->
      let%bind enclosing = env.enclosing in
      climb_nth_env enclosing (depth - 1)

let find_in_environment (n : string) (id : id)
    (var_resolution : Var_resolver.resolution) env =
  let%bind depth = Map.find var_resolution id in
  let%bind env = climb_nth_env env depth in
  Hashtbl.find env.values n

let assign_in_environment (n : string) (id : id) v
    (var_resolution : Var_resolver.resolution) env =
  let%bind depth = Map.find var_resolution id in
  let%bind env = climb_nth_env env depth in
  let%map _ = Hashtbl.find env.values n in
  Hashtbl.set ~key:n ~data:v env.values;
  v

let call_fn var_resolution env eval_exp f args =
  let args = List.map ~f:(fun a -> eval_exp a var_resolution env) args in
  let len = List.length args in
  if not (Int.equal len f.arity) then
    Printf.failwithf "Wrong arity in function call: expected %d, got %d" f.arity
      len ();
  let ret = f.fn args f.decl_environment in

  if f.is_ctor then
    Hashtbl.find f.decl_environment.values "this"
    |> Var_resolver.opt_value ~error:"Unbound `this` in this context"
  else ret

let fn_of_value = function Callable fn -> fn | _ -> assert false

let value_of_fn f = Callable f

let bind_fn env inst c =
  let enclosing = env in
  let env =
    {
      values = Hashtbl.of_alist_exn (module String) [ ("this", inst) ];
      enclosing = Some enclosing;
    }
  in
  c.decl_environment <- env;
  c

let find_method env inst n methods =
  match Hashtbl.find methods n with
  | Some (Callable c) -> Some (bind_fn env inst c |> value_of_fn)
  | _ -> None

let methods_of_class = function
  | VClass (_, _, methods) -> methods
  | _ -> assert false

let find_method_in_class env inst n = methods_of_class >> find_method env inst n

let create_in_current_env n v { values; _ } = Hashtbl.set ~key:n ~data:v values

let eval_callable_of_function var_resolution env eval is_ctor = function
  | Function ({ Lex.kind = Lex.Identifier name; _ }, decl_args, body, _) ->
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
        {
          arity = List.length decl_args;
          name;
          is_ctor = is_ctor name;
          decl_environment = env;
          fn;
        }
      in
      create_in_current_env name (Callable call) env;
      call.decl_environment <- env;
      (name, Callable call)
  | _ -> failwith "Malformed function"

let rec eval_exp exp (var_resolution : Var_resolver.resolution)
    (env : environment) =
  match exp with
  | Super _ -> assert false
  | This (_, id) ->
      find_in_environment "this" id var_resolution env
      |> Var_resolver.opt_value ~error:"Unbound this in this context"
  | Set (lhs, n, rhs) -> (
      let lhs = eval_exp lhs var_resolution env in
      let rhs = eval_exp rhs var_resolution env in
      match lhs with
      | Instance (_, props) ->
          Hashtbl.set ~key:n ~data:rhs props;
          rhs
      | _ ->
          Printf.failwithf
            "Only instances have properties that can be set. Got: %s"
            (value_to_string lhs) () )
  | Get (e, n) -> (
      match eval_exp e var_resolution env with
      | Instance (VClass (c, superclass, methods), fields) as inst ->
          Hashtbl.find fields n
          <|> find_method env inst n methods
          <|> (superclass >>= find_method_in_class env inst n)
          |> Var_resolver.opt_value
               ~error:
                 (Printf.sprintf
                    "Accessing unbound property %s on instance of class %s" n c)
      | other ->
          Printf.failwithf
            "Only instances have properties that can be read. Got: %s"
            (value_to_string other) () )
  | Grouping (e, _) -> eval_exp e var_resolution env
  | Unary (t, e, _) as u -> (
      let v = eval_exp e var_resolution env in
      match (t, v) with
      | Lex.Minus, Number f -> Number (-.f)
      | Lex.Bang, Nil | Lex.Bang, Bool false -> Bool true
      | Lex.Bang, _ -> Bool false
      | _ ->
          Printf.failwithf "Invalid assignment: %s "
            (u |> sexp_of_expr |> Sexp.to_string_hum)
            () )
  | Literal (l, _) -> l
  | LogicalOr (l, r, _) -> (
      let e = eval_exp l var_resolution env in
      match e with Bool false | Nil -> eval_exp r var_resolution env | _ -> e )
  | LogicalAnd (l, r, _) -> (
      let e = eval_exp l var_resolution env in
      match e with Bool false | Nil -> e | _ -> eval_exp r var_resolution env )
  | Variable (Lex.Identifier n, id) ->
      Option.value_exn (find_in_environment n id var_resolution env)
  | Variable _ as v ->
      Printf.failwithf "Invalid variable: %s "
        (v |> sexp_of_expr |> Sexp.to_string_hum)
        ()
  | Assign (Lex.Identifier n, e, id) ->
      let v = eval_exp e var_resolution env in
      Option.value_exn (assign_in_environment n id v var_resolution env)
  | Assign (_, _, _) as a ->
      Printf.failwithf "Invalid assignment: %s "
        (a |> sexp_of_expr |> Sexp.to_string_hum)
        ()
  | Binary (l, t, r, _) as b -> (
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
      | String a, Lex.BangEqual, String b -> Bool (not (String.equal a b))
      | Bool a, Lex.BangEqual, Bool b -> Bool (not (Bool.equal a b))
      | Number a, Lex.EqualEqual, Number b -> Bool (Float.equal a b)
      | String a, Lex.EqualEqual, String b -> Bool (String.equal a b)
      | Bool a, Lex.EqualEqual, Bool b -> Bool (Bool.equal a b)
      | Bool _, Lex.EqualEqual, String _
      | String _, Lex.EqualEqual, Bool _
      | Bool _, Lex.EqualEqual, Number _
      | Number _, Lex.EqualEqual, Bool _
      | Number _, Lex.EqualEqual, String _
      | String _, Lex.EqualEqual, Number _ ->
          Bool false
      | Bool _, Lex.BangEqual, String _
      | String _, Lex.BangEqual, Bool _
      | Bool _, Lex.BangEqual, Number _
      | Number _, Lex.BangEqual, Bool _
      | Number _, Lex.BangEqual, String _
      | String _, Lex.BangEqual, Number _ ->
          Bool true
      | Nil, Lex.EqualEqual, Nil -> Bool true
      | Nil, Lex.EqualEqual, _ -> Bool false
      | _, Lex.EqualEqual, Nil -> Bool false
      | Nil, Lex.BangEqual, Nil -> Bool false
      | Nil, Lex.BangEqual, _ -> Bool true
      | _, Lex.BangEqual, Nil -> Bool true
      | _ ->
          Printf.failwithf "Invalid binary expression: %s"
            (b |> sexp_of_expr |> Sexp.to_string_hum)
            () )
  | Call (callee, _, args, _) ->
      let e = eval_exp callee var_resolution env in
      let f =
        match e with
        | Callable f -> f
        | VClass (n, _, methods) as c -> (
            let inst = Instance (c, empty ()) in
            match Hashtbl.find methods "init" with
            | Some v ->
                let fn = fn_of_value v |> bind_fn env inst in
                let inst = call_fn var_resolution env eval_exp fn args in
                { fn with name = n; is_ctor = true; fn = (fun _ _ -> inst) }
            | None ->
                {
                  arity = 0;
                  name = n;
                  is_ctor = false;
                  decl_environment = env;
                  fn = (fun _ _ -> inst);
                } )
        | _ ->
            Printf.failwithf "Value `%s` cannot be called as a function"
              (value_to_string e) ()
      in
      call_fn var_resolution env eval_exp f args

let rec eval s (var_resolution : Var_resolver.resolution) (env : environment) =
  match s with
  | Class (n, superclass, methods, id) ->
      let superclass =
        Option.map superclass ~f:(fun superclass ->
            let e = eval_exp superclass var_resolution env in
            match e with
            | VClass _ -> e
            | other ->
                Printf.failwithf "Superclass must be a class, got: `%s`"
                  (value_to_string other) ())
      in
      create_in_current_env n Nil env;
      let methods =
        List.map
          ~f:
            (eval_callable_of_function var_resolution env eval
               (String.equal "init"))
          methods
        |> Hashtbl.of_alist_exn (module String)
      in
      let c = VClass (n, superclass, methods) in
      Option.value_exn (assign_in_environment n id c var_resolution env)
  | Expr (e, _) -> eval_exp e var_resolution env
  | Print (e, _) ->
      let v = eval_exp e var_resolution env in
      v |> Parse.value_to_string |> Stdlib.print_endline;
      Nil
  | Var (Lex.Identifier n, e, _) ->
      let e = eval_exp e var_resolution env in
      create_in_current_env n e env;
      e
  | Var (_, _, _) as v ->
      Printf.failwithf "Invalid variable declaration: %s "
        (v |> sexp_of_statement |> Sexp.to_string_hum)
        ()
  | Block (stmts, _) ->
      let enclosing = env in
      let env = { values = empty (); enclosing = Some enclosing } in
      Array.iter ~f:(fun s -> eval s var_resolution env |> ignore) stmts;
      Nil
  | Return (_, expr, _) ->
      let v = eval_exp expr var_resolution env in
      raise (FunctionReturn v)
  | Function _ as f ->
      eval_callable_of_function var_resolution env eval (fun _ -> false) f
      |> ignore;
      Nil
  | IfElseStmt (e, then_stmt, else_stmt, _) -> (
      let e = eval_exp e var_resolution env in
      match e with
      | Bool false | Nil -> eval else_stmt var_resolution env
      | _ -> eval then_stmt var_resolution env )
  | IfStmt (e, then_stmt, _) -> (
      let e = eval_exp e var_resolution env in
      match e with
      | Bool false | Nil -> Nil
      | _ -> eval then_stmt var_resolution env )
  | WhileStmt _ -> eval_while s var_resolution env

and eval_while w var_resolution env =
  match w with
  | WhileStmt (e, s, _) -> (
      let e = eval_exp e var_resolution env in
      match e with
      | Bool false | Nil -> Nil
      | _ ->
          eval s var_resolution env |> ignore;
          eval_while w var_resolution env )
  | _ ->
      Printf.failwithf "Invalid while loop: %s "
        (w |> sexp_of_statement |> Sexp.to_string_hum)
        ()

let interpret (var_resolution : Var_resolver.resolution) (env : environment)
    stmts =
  try
    stmts |> List.to_array |> Array.map ~f:(fun s -> eval s var_resolution env)
    |> fun stmts -> Ok stmts
  with Failure err -> Result.Error [ err ]
