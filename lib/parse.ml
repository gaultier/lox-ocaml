open Sexplib.Std

type literal_value = Bool of bool | Number of float | Nil | String of string
[@@deriving sexp]

type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of literal_value
  | Unary of Lex.lex_token * expr
[@@deriving sexp]

let rec primary = function
  | tokens -> (
    match tokens with
    | [] ->
        failwith "No more tokens to match"
    | Lex.False :: rest ->
        (Literal (Bool false), rest)
    | Lex.True :: rest ->
        (Literal (Bool true), rest)
    | Lex.Nil :: rest ->
        (Literal Nil, rest)
    | Lex.Number f :: rest ->
        (Literal (Number f), rest)
    | Lex.String s :: rest ->
        (Literal (String s), rest)
    | Lex.ParenLeft :: rest -> (
        let e, rrest = expression rest in
        match rrest with
        | Lex.ParenRight :: rrrest ->
            (Grouping e, rrrest)
        | _ ->
            failwith "Missing closing parenthesis" )
    | x :: _ ->
        failwith
          ( "Not a primary: "
          ^ Base.Sexp.to_string_hum (Lex.sexp_of_lex_token x) ) )

and unary = function
  | tokens -> (
    match tokens with
    | (Lex.Bang as t) :: rest | (Lex.Minus as t) :: rest ->
        let right, rrest = unary rest in
        (Unary (t, right), rrest)
    | _ ->
        primary tokens )

and multiplication = function
  | tokens -> (
      let left, rest = unary tokens in
      match rest with
      | (Lex.Star as t) :: rrest | (Lex.Slash as t) :: rrest ->
          let right, rrrest = multiplication rrest in
          (Binary (left, t, right), rrrest)
      | _ ->
          (left, rest) )

and addition = function
  | tokens -> (
      let left, rest = multiplication tokens in
      match rest with
      | (Lex.Plus as t) :: rrest | (Lex.Minus as t) :: rrest ->
          let right, rrrest = addition rrest in
          (Binary (left, t, right), rrrest)
      | _ ->
          (left, rest) )

and comparison = function
  | tokens -> (
      let left, rest = addition tokens in
      match rest with
      | (Lex.Greater as t) :: rrest
      | (Lex.GreaterEqual as t) :: rrest
      | (Lex.Less as t) :: rrest
      | (Lex.LessEqual as t) :: rrest ->
          let right, rrrest = comparison rrest in
          (Binary (left, t, right), rrrest)
      | _ ->
          (left, rest) )

and equality = function
  | tokens -> (
      let left, rest = comparison tokens in
      match rest with
      | (Lex.BangEqual as t) :: rrest | (Lex.EqualEqual as t) :: rrest ->
          let right, rrrest = equality rrest in
          (Binary (left, t, right), rrrest)
      | _ ->
          (left, rest) )

and expression = function tokens -> equality tokens

let parse tokens = expression tokens |> fst

let%test _ = expression [Lex.False] = (Literal (Bool false), [])

let%test _ = expression [Lex.True] = (Literal (Bool true), [])

let%test _ = expression [Lex.Nil] = (Literal Nil, [])

let%test _ = expression [Lex.Number 3.] = (Literal (Number 3.), [])

let%test _ = expression [Lex.String "ab"] = (Literal (String "ab"), [])

let%test _ =
  unary [Lex.Bang; Lex.Number 1.] = (Unary (Lex.Bang, Literal (Number 1.)), [])

let%test _ =
  unary [Lex.Minus; Lex.Number 1.]
  = (Unary (Lex.Minus, Literal (Number 1.)), [])

let%test _ = expression [Lex.Number 1.] = (Literal (Number 1.), [])

let%test _ = expression [Lex.Number 1.] = (Literal (Number 1.), [])

let%test _ =
  expression [Lex.Number 1.; Lex.Star; Lex.Number 2.]
  = (Binary (Literal (Number 1.), Lex.Star, Literal (Number 2.)), [])

let%test _ =
  expression [Lex.Number 1.; Lex.Star; Lex.Minus; Lex.Number 2.]
  = ( Binary
        (Literal (Number 1.), Lex.Star, Unary (Lex.Minus, Literal (Number 2.)))
    , [] )

let%test _ =
  expression [Lex.Number 1.; Lex.Slash; Lex.Minus; Lex.Number 2.]
  = ( Binary
        (Literal (Number 1.), Lex.Slash, Unary (Lex.Minus, Literal (Number 2.)))
    , [] )

let%test _ =
  expression [Lex.Number 1.; Lex.Plus; Lex.Minus; Lex.Number 2.]
  = ( Binary
        (Literal (Number 1.), Lex.Plus, Unary (Lex.Minus, Literal (Number 2.)))
    , [] )

let%test _ =
  "1 + -2" |> Lex.lex |> expression
  = ( Binary
        (Literal (Number 1.), Lex.Plus, Unary (Lex.Minus, Literal (Number 2.)))
    , [] )

let%test _ =
  "1 < -2" |> Lex.lex |> expression
  = ( Binary
        (Literal (Number 1.), Lex.Less, Unary (Lex.Minus, Literal (Number 2.)))
    , [] )

let%test _ =
  "1 == -2" |> Lex.lex |> expression
  = ( Binary
        ( Literal (Number 1.)
        , Lex.EqualEqual
        , Unary (Lex.Minus, Literal (Number 2.)) )
    , [] )

let%test _ =
  "1 != 3" |> Lex.lex |> expression
  = (Binary (Literal (Number 1.), Lex.BangEqual, Literal (Number 3.)), [])

let%test _ =
  "1 != 3 == -2" |> Lex.lex |> expression
  = ( Binary
        ( Literal (Number 1.)
        , Lex.BangEqual
        , Binary
            ( Literal (Number 3.)
            , Lex.EqualEqual
            , Unary (Lex.Minus, Literal (Number 2.)) ) )
    , [] )

let%test _ =
  "(1 != 3) == -2" |> Lex.lex |> expression
  = ( Binary
        ( Grouping
            (Binary (Literal (Number 1.), Lex.BangEqual, Literal (Number 3.)))
        , Lex.EqualEqual
        , Unary (Lex.Minus, Literal (Number 2.)) )
    , [] )

let%test _ =
  "2 >= (1 + 1) " |> Lex.lex |> expression
  = ( Binary
        ( Literal (Number 2.)
        , Lex.GreaterEqual
        , Grouping
            (Binary (Literal (Number 1.), Lex.Plus, Literal (Number 1.))) )
    , [] )
