type literal_value = false | true | EFloat of float | Nil | EString of string

type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of literal_value
  | Unary of Lex.lex_token * expr
  | Error

let primary tokens =
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
  | _ :: rest ->
      (* TODO: parenthesized expression *)
      (Literal Nil, rest)

let rec unary tokens =
  match tokens with
  | Lex.Bang :: rest ->
      let right, rrest = unary rest in
      (Unary (Lex.Bang, right), rrest)
  | Lex.Minus :: rest ->
      let right, rrest = unary rest in
      (Unary (Lex.Minus, right), rrest)
  | _ ->
      primary tokens

let multiplication tokens =
  let left, rest = unary tokens in
  match rest with
  | Lex.Star :: rrest ->
      let right, rrrest = unary rrest in
      (Binary (left, Lex.Star, right), rrrest)
  | Lex.Slash :: rrest ->
      let right, rrrest = unary rrest in
      (Binary (left, Lex.Slash, right), rrrest)
  | _ ->
      (left, rest)

let addition tokens =
  let left, rest = multiplication tokens in
  match rest with
  | Lex.Plus :: rrest ->
      let right, rrrest = multiplication rrest in
      (Binary (left, Lex.Plus, right), rrrest)
  | Lex.Minus :: rrest ->
      let right, rrrest = multiplication rrest in
      (Binary (left, Lex.Minus, right), rrrest)
  | _ ->
      (left, rest)

let comparison tokens =
  let left, rest = addition tokens in
  match rest with
  | (Lex.Greater as t) :: rrest ->
      let right, rrrest = addition rrest in
      (Binary (left, t, right), rrrest)
  | (Lex.GreaterEqual as t) :: rrest ->
      let right, rrrest = addition rrest in
      (Binary (left, t, right), rrrest)
  | (Lex.Less as t) :: rrest ->
      let right, rrrest = addition rrest in
      (Binary (left, t, right), rrrest)
  | (Lex.LessEqual as t) :: rrest ->
      let right, rrrest = addition rrest in
      (Binary (left, t, right), rrrest)
  | _ ->
      (left, rest)

let equality tokens =
  let left, rest = comparison tokens in
  match rest with
  | (Lex.BangEqual as t) :: rrest ->
      let right, rrrest = comparison rrest in
      (Binary (left, t, right), rrrest)
  | (Lex.EqualEqual as t) :: rrest ->
      let right, rrrest = comparison rrest in
      (Binary (left, t, right), rrrest)
  | _ ->
      (left, rest)

let expression tokens = equality tokens

let rec expr_to_s e =
  match e with
  | Binary (l, t, r) ->
      "(Binary " ^ expr_to_s l ^ " " ^ Lex.lex_token_to_s t ^ " " ^ expr_to_s r
      ^ ")"
  | Grouping m ->
      "(Grouping " ^ expr_to_s m ^ ")"
  | Literal v ->
      let literal_value_s =
        match v with
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
      in
      "(Literal " ^ literal_value_s ^ ")"
  | Unary (t, r) ->
      "(Unary " ^ Lex.lex_token_to_s t ^ " " ^ expr_to_s r ^ ")"
  | Error ->
      "(Error)"

let%test _ = primary [Lex.False] = (Literal false, [])

let%test _ = primary [Lex.True] = (Literal true, [])

let%test _ = primary [Lex.Nil] = (Literal Nil, [])

let%test _ = primary [Lex.LexNumber 3.] = (Literal (EFloat 3.), [])

let%test _ = primary [Lex.LexString ['a'; 'b']] = (Literal (EString "ab"), [])

let%test _ =
  unary [Lex.Bang; Lex.LexNumber 1.]
  = (Unary (Lex.Bang, Literal (EFloat 1.)), [])

let%test _ =
  unary [Lex.Minus; Lex.LexNumber 1.]
  = (Unary (Lex.Minus, Literal (EFloat 1.)), [])

let%test _ = primary [Lex.LexNumber 1.] = (Literal (EFloat 1.), [])

let%test _ = multiplication [Lex.LexNumber 1.] = (Literal (EFloat 1.), [])

let%test _ =
  multiplication [Lex.LexNumber 1.; Lex.Star; Lex.LexNumber 2.]
  = (Binary (Literal (EFloat 1.), Lex.Star, Literal (EFloat 2.)), [])

let%test _ =
  multiplication [Lex.LexNumber 1.; Lex.Star; Lex.Minus; Lex.LexNumber 2.]
  = ( Binary
        (Literal (EFloat 1.), Lex.Star, Unary (Lex.Minus, Literal (EFloat 2.)))
    , [] )

let%test _ =
  multiplication [Lex.LexNumber 1.; Lex.Slash; Lex.Minus; Lex.LexNumber 2.]
  = ( Binary
        (Literal (EFloat 1.), Lex.Slash, Unary (Lex.Minus, Literal (EFloat 2.)))
    , [] )

let%test _ =
  addition [Lex.LexNumber 1.; Lex.Plus; Lex.Minus; Lex.LexNumber 2.]
  = ( Binary
        (Literal (EFloat 1.), Lex.Plus, Unary (Lex.Minus, Literal (EFloat 2.)))
    , [] )

let%test _ =
  "1 + -2" |> Lex.lex |> addition
  = ( Binary
        (Literal (EFloat 1.), Lex.Plus, Unary (Lex.Minus, Literal (EFloat 2.)))
    , [] )

let%test _ =
  "1 < -2" |> Lex.lex |> comparison
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
        ( Binary (Literal (EFloat 1.), Lex.BangEqual, Literal (EFloat 3.))
        , EqualEqual
        , Unary (Lex.Minus, Literal (EFloat 2.)) )
  , [] );;

"1 != 3 == -2" |> Lex.lex |> expression |> fst |> expr_to_s |> print_endline
