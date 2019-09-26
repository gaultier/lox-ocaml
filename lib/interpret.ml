open Parse

let is_truthy e =
  match e with Bool false | Nil -> Bool false | _ -> Bool true

let bool_not b = if b == Bool true then Bool false else Bool true

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
  | Binary (Literal (Number a), Lex.Plus, Literal (Number b)) ->
      Number (a +. b)
  | Binary (Literal (Number a), Lex.Minus, Literal (Number b)) ->
      Number (a -. b)
  | Binary (Literal (Number a), Lex.Slash, Literal (Number b)) ->
      Number (a /. b)
  | Binary (Literal (Number a), Lex.Star, Literal (Number b)) ->
      Number (a *. b)
  | Binary (Literal (Number a), Lex.Less, Literal (Number b)) ->
      Bool (a < b)
  | Binary (Literal (Number a), Lex.LessEqual, Literal (Number b)) ->
      Bool (a <= b)
  | Binary (Literal (Number a), Lex.Greater, Literal (Number b)) ->
      Bool (a > b)
  | Binary (Literal (Number a), Lex.GreaterEqual, Literal (Number b)) ->
      Bool (a >= b)
  | Binary (Literal (Number a), Lex.BangEqual, Literal (Number b)) ->
      Bool (a != b)
  | Binary (Literal (Number a), Lex.EqualEqual, Literal (Number b)) ->
      Bool (Float.equal a b)
  | Binary (Literal (String a), Lex.EqualEqual, Literal (String b)) ->
      Bool (String.equal a b)
  | Binary (Literal (Bool a), Lex.EqualEqual, Literal (Bool b)) ->
      Bool (a == b)
  | Binary (Literal Nil, Lex.EqualEqual, Literal Nil) ->
      Bool true
  | Binary (Literal Nil, Lex.EqualEqual, _) ->
      Bool false
  | Binary (_, Lex.EqualEqual, Literal Nil) ->
      Bool false
  | _ ->
      failwith
        ( "Not implemented yet: "
        ^ Sexplib.Std.string_of_sexp (sexp_of_expr exp) )

let%test _ =
  "(-1 + 3 * 5)" |> Lex.lex |> expression |> fst |> eval = Number 14.

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
