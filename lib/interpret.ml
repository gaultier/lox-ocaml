open Parse

let rec eval exp =
  match exp with
  | Grouping e ->
      eval e
  | Unary (t, e) -> (
      let v = eval e in
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
            ^ Base.Sexp.to_string_hum (sexp_of_expr e) ) )
  | Literal l ->
      l
  | Binary (l, t, r) -> (
      let x = eval l and y = eval r in
      match (x, t, y) with
      | Number a, Lex.Plus, Number b ->
          Number (a +. b)
      | Number a, Lex.Minus, Number b ->
          Number (a -. b)
      | Number _, Lex.Slash, Number 0. ->
          failwith
            ( "Division by zero not allowed: "
            ^ Base.Sexp.to_string_hum (sexp_of_expr exp) )
      | Number a, Lex.Slash, Number b ->
          Number (a /. b)
      | Number a, Lex.Star, Number b ->
          Number (a *. b)
      | Number a, Lex.Less, Number b ->
          Bool (a < b)
      | Number a, Lex.LessEqual, Number b ->
          Bool (a <= b)
      | Number a, Lex.Greater, Number b ->
          Bool (a > b)
      | Number a, Lex.GreaterEqual, Number b ->
          Bool (a >= b)
      | Number a, Lex.BangEqual, Number b ->
          Bool (a != b)
      | Number a, Lex.EqualEqual, Number b ->
          Bool (Float.equal a b)
      | String a, Lex.EqualEqual, String b ->
          Bool (String.equal a b)
      | String a, Lex.Plus, String b ->
          String (a ^ b)
      | Bool a, Lex.EqualEqual, Bool b ->
          Bool (a == b)
      | Nil, Lex.EqualEqual, Nil ->
          Bool true
      | Nil, Lex.EqualEqual, _ ->
          Bool false
      | _, Lex.EqualEqual, Nil ->
          Bool false
      | _ ->
          failwith
            ( "Binary expression not allowed: "
            ^ Base.Sexp.to_string_hum (sexp_of_expr exp) ) )

let%test _ = "1 + 3" |> Lex.lex |> expression |> fst |> eval = Number 4.

let%test _ = "-1 + 3" |> Lex.lex |> expression |> fst |> eval = Number 2.

let%test _ =
  "(-1 + 3 * 5)" |> Lex.lex |> expression |> fst |> eval = Number 14.

let%test _ =
  "(-1 + 3 * 5) == (2*5 + 4)" |> Lex.lex |> expression |> fst |> eval
  = Bool true

let%test _ = "10/5" |> Lex.lex |> expression |> fst |> eval = Number 2.

let%test _ = "!true" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ = "!false" |> Lex.lex |> expression |> fst |> eval = Bool true

let%test _ = "!(1 == 1)" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ = "!nil" |> Lex.lex |> expression |> fst |> eval = Bool true

let%test _ = "!!nil" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ =
  "\"hey\" == \"hello\"" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ =
  "\"hey\" == \"hey\"" |> Lex.lex |> expression |> fst |> eval = Bool true

let%test _ =
  "\"hel\" + \"lo\"" |> Lex.lex |> expression |> fst |> eval = String "hello"

let%test _ = "1 >= 2" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ = "1 > 2" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ = "2 > 2" |> Lex.lex |> expression |> fst |> eval = Bool false

let%test _ = "1 <= 2" |> Lex.lex |> expression |> fst |> eval = Bool true

let%test _ = "1 < 2" |> Lex.lex |> expression |> fst |> eval = Bool true

let%test _ = "2 < 2" |> Lex.lex |> expression |> fst |> eval = Bool false
