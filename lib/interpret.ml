open Parse
module StringMap = Map.Make (String)

let print e =
  match e with
  | String s ->
      Printf.printf "%s\n" s
  | Number f ->
      Printf.printf "%f\n" f
  | Bool true ->
      Printf.printf "true\n"
  | Bool false ->
      Printf.printf "false\n"
  | Nil ->
      Printf.printf "nil\n"

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
            failwith
              ( "Unary expression not allowed: "
              ^ Base.Sexp.to_string_hum (sexp_of_expr e) )
      in
      (res, env)
  | Literal l ->
      (l, env)
  | Variable (Lex.Identifier n) -> (
    match StringMap.find_opt n env with
    | Some v ->
        (v, env)
    | None ->
        failwith ("Accessing unkown variable `" ^ n ^ "`") )
  | Variable _ ->
      failwith "Badly constructed var"
  | Binary (l, t, r) -> (
      let l, env = eval_exp l env in
      let r, env = eval_exp r env in
      match (l, t, r) with
      | Number a, Lex.Plus, Number b ->
          (Number (a +. b), env)
      | Number a, Lex.Minus, Number b ->
          (Number (a -. b), env)
      | Number _, Lex.Slash, Number 0. ->
          failwith
            ( "Division by zero not allowed: "
            ^ Base.Sexp.to_string_hum (sexp_of_expr exp) )
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
      | String a, Lex.Plus, String b ->
          (String (a ^ b), env)
      | Bool a, Lex.EqualEqual, Bool b ->
          (Bool (a == b), env)
      | Nil, Lex.EqualEqual, Nil ->
          (Bool true, env)
      | Nil, Lex.EqualEqual, _ ->
          (Bool false, env)
      | _, Lex.EqualEqual, Nil ->
          (Bool false, env)
      | _ ->
          failwith
            ( "Binary expression not allowed: "
            ^ Base.Sexp.to_string_hum (sexp_of_expr exp) ) )

let eval s env =
  match s with
  | Expr e ->
      eval_exp e env
  | Print e ->
      let v, env = eval_exp e env in
      print v ; (Nil, env)
  | Var (Lex.Identifier n, e) ->
      let e, env = eval_exp e env in
      let env = StringMap.add n e env in
      (e, env)
  | Var _ ->
      failwith "Badly constructed var"

let interpret env stmts =
  Base.Array.fold stmts
    ~init:([||], env)
    ~f:(fun (acc, env) s ->
      let e, env = eval s env in
      (Array.append acc [|e|], env))

(* let%test _ = "1 + 3" |> Lex.lex |> expression |> fst |> eval_exp = Number 4. *)

(* let%test _ = "-1 + 3" |> Lex.lex |> expression |> fst |> eval_exp = Number 2. *)

(* let%test _ = *)
(*   "(-1 + 3 * 5)" |> Lex.lex |> expression |> fst |> eval_exp = Number 14. *)

(* let%test _ = *)
(*   "(-1 + 3 * 5) == (2*5 + 4)" |> Lex.lex |> expression |> fst |> eval_exp *)
(*   = Bool true *)

(* let%test _ = "10/5" |> Lex.lex |> expression |> fst |> eval_exp = Number 2. *)

(* let%test _ = "!true" |> Lex.lex |> expression |> fst |> eval_exp = Bool false *)

(* let%test _ = "!false" |> Lex.lex |> expression |> fst |> eval_exp = Bool true *)

(* let%test _ = *)
(*   "!(1 == 1)" |> Lex.lex |> expression |> fst |> eval_exp = Bool false *)

(* let%test _ = "!nil" |> Lex.lex |> expression |> fst |> eval_exp = Bool true *)

(* let%test _ = "!!nil" |> Lex.lex |> expression |> fst |> eval_exp = Bool false *)

(* let%test _ = *)
(*   "\"hey\" == \"hello\"" |> Lex.lex |> expression |> fst |> eval_exp *)
(*   = Bool false *)

(* let%test _ = *)
(*   "\"hey\" == \"hey\"" |> Lex.lex |> expression |> fst |> eval_exp = Bool true *)

(* let%test _ = *)
(*   "\"hel\" + \"lo\"" |> Lex.lex |> expression |> fst |> eval_exp *)
(*   = String "hello" *)

(* let%test _ = "1 >= 2" |> Lex.lex |> expression |> fst |> eval_exp = Bool false *)

(* let%test _ = "1 > 2" |> Lex.lex |> expression |> fst |> eval_exp = Bool false *)

(* let%test _ = "2 > 2" |> Lex.lex |> expression |> fst |> eval_exp = Bool false *)

(* let%test _ = "1 <= 2" |> Lex.lex |> expression |> fst |> eval_exp = Bool true *)

(* let%test _ = "1 < 2" |> Lex.lex |> expression |> fst |> eval_exp = Bool true *)

(* let%test _ = "2 < 2" |> Lex.lex |> expression |> fst |> eval_exp = Bool false *)
