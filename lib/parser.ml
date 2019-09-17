type literal_value = false | true | EFloat of float | Nil | EString of string

type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of literal_value
  | Unary of Lex.lex_token * expr

let grouping _ = Grouping (Literal Nil)

let parse_r acc rest =
  match rest with
  | [] ->
      acc
  | Lex.False :: _ ->
      Literal false
  | Lex.True :: _ ->
      Literal true
  | Lex.Nil :: _ ->
      Literal Nil
  | Lex.LexNumber f :: _ ->
      Literal (EFloat f)
  | Lex.LexString s :: _ ->
      Literal (EString (Base.String.of_char_list s))
  | Lex.LeftParen :: irest ->
      grouping irest
  | _ ->
      Literal Nil

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

let%test _ = parse_r (Literal (EFloat 99.)) [Lex.False] = Literal false

let%test _ = parse_r (Literal (EFloat 99.)) [Lex.True] = Literal true

let%test _ = parse_r (Literal (EFloat 99.)) [Lex.Nil] = Literal Nil

let%test _ =
  parse_r (Literal (EFloat 99.)) [Lex.LexNumber 3.] = Literal (EFloat 3.)

let%test _ =
  parse_r (Literal (EFloat 99.)) [Lex.LexString ['a'; 'b']]
  = Literal (EString "ab")
