let efloat_op_float a b op =
  match (a, b) with
  | Parse.Number x, Parse.Number y ->
      Parse.Number (op x y)
  | _ ->
      Parse.Nil

let efloat_op_bool a b op =
  match (a, b) with
  | Parse.Number x, Parse.Number y ->
      if op x y then Parse.True else Parse.False
  | _ ->
      Parse.Nil

let is_truthy e =
  match e with Parse.False | Parse.Nil -> Parse.False | _ -> Parse.True

let bool_not b = if b == Parse.True then Parse.False else Parse.True

let is_equal l1 l2 =
  match (l1, l2) with
  | Parse.Nil, Parse.Nil ->
      Parse.True
  | Parse.Nil, _ | _, Parse.Nil ->
      Parse.False
  | Parse.Number _, Parse.Number _ ->
      efloat_op_bool l1 l2 Float.equal
  | Parse.String a, Parse.String b ->
      if String.equal a b then Parse.True else Parse.False
  | _ ->
      Parse.False

let rec eval exp =
  match exp with
  | Parse.Grouping e ->
      eval e
  | Parse.Unary (t, e) -> (
      let v = eval e in
      match (v, t) with
      | Parse.Number f, Lex.Minus ->
          Parse.Number (-.f)
      | _, Lex.Bang ->
          bool_not (is_truthy v)
      | _ ->
          v )
  | Parse.Literal l ->
      l
  | Parse.Binary (left, Lex.Plus, right) ->
      efloat_op_float (eval left) (eval right) ( +. )
  | Parse.Binary (left, Lex.Minus, right) ->
      efloat_op_float (eval left) (eval right) ( -. )
  | Parse.Binary (left, Lex.Slash, right) ->
      efloat_op_float (eval left) (eval right) ( /. )
  | Parse.Binary (left, Lex.Star, right) ->
      efloat_op_float (eval left) (eval right) ( *. )
  | Parse.Binary (left, Lex.Less, right) ->
      efloat_op_bool (eval left) (eval right) ( < )
  | Parse.Binary (left, Lex.LessEqual, right) ->
      efloat_op_bool (eval left) (eval right) ( <= )
  | Parse.Binary (left, Lex.Greater, right) ->
      efloat_op_bool (eval left) (eval right) ( > )
  | Parse.Binary (left, Lex.GreaterEqual, right) ->
      efloat_op_bool (eval left) (eval right) ( >= )
  | Parse.Binary (left, Lex.BangEqual, right) ->
      bool_not (is_equal (eval left) (eval right))
  | Parse.Binary (left, Lex.EqualEqual, right) ->
      is_equal (eval left) (eval right)
  | _ ->
      Nil

let%test _ =
  "(-1 + 3 * 5) == (2*5 + 4)" |> Lex.lex |> Parse.expression |> fst |> eval
  = True

let%test _ = "!true" |> Lex.lex |> Parse.expression |> fst |> eval = False

let%test _ = "!false" |> Lex.lex |> Parse.expression |> fst |> eval = True

let%test _ = "!(1 == 1)" |> Lex.lex |> Parse.expression |> fst |> eval = False

let%test _ = "!nil" |> Lex.lex |> Parse.expression |> fst |> eval = True

let%test _ = "!!nil" |> Lex.lex |> Parse.expression |> fst |> eval = False

let%test _ =
  "\"hey\" == \"hello\"" |> Lex.lex |> Parse.expression |> fst |> eval = False

let%test _ =
  "\"hey\" == \"hey\"" |> Lex.lex |> Parse.expression |> fst |> eval = True
