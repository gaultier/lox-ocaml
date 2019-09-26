open Parse

let efloat_op_float a b op =
  match (a, b) with Number x, Number y -> Number (op x y) | _ -> Nil

let efloat_op_bool a b op =
  match (a, b) with
  | Number x, Number y ->
      if op x y then Bool true else Bool false
  | _ ->
      Nil

let is_truthy e =
  match e with Bool false | Nil -> Bool false | _ -> Bool true

let bool_not b = if b == Bool true then Bool false else Bool true

let is_equal l1 l2 =
  match (l1, l2) with
  | Nil, Nil ->
      Bool true
  | Nil, _ | _, Nil ->
      Bool false
  | Number _, Number _ ->
      efloat_op_bool l1 l2 Float.equal
  | String a, String b ->
      if String.equal a b then Bool true else Bool false
  | _ ->
      Bool false

let rec eval exp =
  match exp with
  | Grouping e ->
      eval e
  | Unary (t, e) -> (
      let v = eval e in
      match (v, t) with
      | Number f, Lex.Minus ->
          Number (-.f)
      | _, Lex.Bang ->
          bool_not (is_truthy v)
      | _ ->
          v )
  | Literal l ->
      l
  | Binary (left, Lex.Plus, right) ->
      efloat_op_float (eval left) (eval right) ( +. )
  | Binary (Literal (Number a), Lex.Minus, Literal (Number b)) ->
      Number (a -. b)
  | Binary (left, Lex.Slash, right) ->
      efloat_op_float (eval left) (eval right) ( /. )
  | Binary (left, Lex.Star, right) ->
      efloat_op_float (eval left) (eval right) ( *. )
  | Binary (left, Lex.Less, right) ->
      efloat_op_bool (eval left) (eval right) ( < )
  | Binary (left, Lex.LessEqual, right) ->
      efloat_op_bool (eval left) (eval right) ( <= )
  | Binary (left, Lex.Greater, right) ->
      efloat_op_bool (eval left) (eval right) ( > )
  | Binary (left, Lex.GreaterEqual, right) ->
      efloat_op_bool (eval left) (eval right) ( >= )
  | Binary (left, Lex.BangEqual, right) ->
      bool_not (is_equal (eval left) (eval right))
  | Binary (left, Lex.EqualEqual, right) ->
      is_equal (eval left) (eval right)
  | _ ->
      Nil

let%test _ =
  "(-1 + 3 * 5) == (2*5 + 4)" |> Lex.lex |> expression |> fst |> eval
  = Bool true

let%test _ = "!true" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ = "!false" |> Lex.lex |> expression |> fst |> eval = Bool true

let%test _ = "!(1 == 1)" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ = "!nil" |> Lex.lex |> expression |> fst |> eval = Bool true

let%test _ = "!!nil" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ =
  "\"hey\" == \"hello\"" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ =
  "\"hey\" == \"hey\"" |> Lex.lex |> expression |> fst |> eval = Bool true
