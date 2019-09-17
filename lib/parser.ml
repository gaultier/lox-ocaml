type literal_value = false | true | Float of float | Nil

type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of literal_value
  | Unary of Lex.lex_token * expr

let parse_r acc rest =
  match rest with
  | [] ->
      acc
  | Lex.False :: _ ->
      Literal (Float 0.)
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
        | Float f ->
            Float.to_string f
        | Nil ->
            "nil"
      in
      "(Literal " ^ literal_value_s ^ ")"
  | Unary (t, r) ->
      "(Unary " ^ Lex.lex_token_to_s t ^ " " ^ expr_to_s r ^ ")"

let%test _ = parse_r (Literal (Float 99.)) [Lex.False] = Literal (Float 0.)
