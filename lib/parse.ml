type literal_value = false | true | EFloat of float | Nil | EString of string

type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of literal_value
  | Unary of Lex.lex_token * expr
  | Error

let rec primary = function
  | tokens -> (
    match tokens with
    | [] ->
        (Error, [])
    | Lex.False :: rest ->
        (Literal false, rest)
    | Lex.True :: rest ->
        (Literal true, rest)
    | Lex.Nil :: rest ->
        (Literal Nil, rest)
    | Lex.LexNumber f :: rest ->
        (Literal (EFloat f), rest)
    | Lex.LexString s :: rest ->
        (Literal (EString (Base.String.of_char_list s)), rest)
    | Lex.ParenLeft :: rest -> (
        let e, rrest = expression rest in
        match rrest with
        | Lex.ParenRight :: rrrest ->
            (Grouping e, rrrest)
        | _ ->
            (Error, rrest) )
    | _ :: rest ->
        (Error, rest) )

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

let literal_to_s l =
  match l with
  | false ->
      "false"
  | true ->
      "true"
  | EFloat f ->
      Float.to_string f
  | EString s ->
      s
  | Nil ->
      "nil"

let rec expr_to_s e =
  match e with
  | Binary (l, t, r) ->
      "(" ^ Lex.lex_token_to_s t ^ " " ^ expr_to_s l ^ " " ^ expr_to_s r ^ ")"
  | Grouping m ->
      "(Grouping " ^ expr_to_s m ^ ")"
  | Literal v ->
      literal_to_s v
  | Unary (t, r) ->
      "(" ^ Lex.lex_token_to_s t ^ " " ^ expr_to_s r ^ ")"
  | Error ->
      "(Error)"

let%test _ = expression [Lex.False] = (Literal false, [])

let%test _ = expression [Lex.True] = (Literal true, [])

let%test _ = expression [Lex.Nil] = (Literal Nil, [])

let%test _ = expression [Lex.LexNumber 3.] = (Literal (EFloat 3.), [])

let%test _ =
  expression [Lex.LexString ['a'; 'b']] = (Literal (EString "ab"), [])

let%test _ =
  unary [Lex.Bang; Lex.LexNumber 1.]
  = (Unary (Lex.Bang, Literal (EFloat 1.)), [])

let%test _ =
  unary [Lex.Minus; Lex.LexNumber 1.]
  = (Unary (Lex.Minus, Literal (EFloat 1.)), [])

let%test _ = expression [Lex.LexNumber 1.] = (Literal (EFloat 1.), [])

let%test _ = expression [Lex.LexNumber 1.] = (Literal (EFloat 1.), [])

let%test _ =
  expression [Lex.LexNumber 1.; Lex.Star; Lex.LexNumber 2.]
  = (Binary (Literal (EFloat 1.), Lex.Star, Literal (EFloat 2.)), [])

let%test _ =
  expression [Lex.LexNumber 1.; Lex.Star; Lex.Minus; Lex.LexNumber 2.]
  = ( Binary
        (Literal (EFloat 1.), Lex.Star, Unary (Lex.Minus, Literal (EFloat 2.)))
    , [] )

let%test _ =
  expression [Lex.LexNumber 1.; Lex.Slash; Lex.Minus; Lex.LexNumber 2.]
  = ( Binary
        (Literal (EFloat 1.), Lex.Slash, Unary (Lex.Minus, Literal (EFloat 2.)))
    , [] )

let%test _ =
  expression [Lex.LexNumber 1.; Lex.Plus; Lex.Minus; Lex.LexNumber 2.]
  = ( Binary
        (Literal (EFloat 1.), Lex.Plus, Unary (Lex.Minus, Literal (EFloat 2.)))
    , [] )

let%test _ =
  "1 + -2" |> Lex.lex |> expression
  = ( Binary
        (Literal (EFloat 1.), Lex.Plus, Unary (Lex.Minus, Literal (EFloat 2.)))
    , [] )

let%test _ =
  "1 < -2" |> Lex.lex |> expression
  = ( Binary
        (Literal (EFloat 1.), Lex.Less, Unary (Lex.Minus, Literal (EFloat 2.)))
    , [] )

let%test _ =
  "1 == -2" |> Lex.lex |> expression
  = ( Binary
        ( Literal (EFloat 1.)
        , Lex.EqualEqual
        , Unary (Lex.Minus, Literal (EFloat 2.)) )
    , [] )

let%test _ =
  "1 != 3" |> Lex.lex |> expression
  = (Binary (Literal (EFloat 1.), Lex.BangEqual, Literal (EFloat 3.)), [])

let%test _ =
  "1 != 3 == -2" |> Lex.lex |> expression
  = ( Binary
        ( Literal (EFloat 1.)
        , Lex.BangEqual
        , Binary
            ( Literal (EFloat 3.)
            , Lex.EqualEqual
            , Unary (Lex.Minus, Literal (EFloat 2.)) ) )
    , [] )

let%test _ =
  "(1 != 3) == -2" |> Lex.lex |> expression
  = ( Binary
        ( Grouping
            (Binary (Literal (EFloat 1.), Lex.BangEqual, Literal (EFloat 3.)))
        , Lex.EqualEqual
        , Unary (Lex.Minus, Literal (EFloat 2.)) )
    , [] )

let%test _ =
  "2 >= (1 + 1) " |> Lex.lex |> expression
  = ( Binary
        ( Literal (EFloat 2.)
        , Lex.GreaterEqual
        , Grouping
            (Binary (Literal (EFloat 1.), Lex.Plus, Literal (EFloat 1.))) )
    , [] )
