type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of float
  | Unary of Lex.lex_token * expr

let parse_r acc rest = match rest with 
| [] -> acc
| Lex.False :: _ -> Literal (0.)
| _ -> Literal(-1.)

let rec expr_to_s e =
  match e with
  | Binary (l, t, r) ->
      "(Binary " ^ expr_to_s l ^ " " ^ Lex.lex_token_to_s t ^ " " ^ expr_to_s r ^ ")"
  | Grouping m ->
      "(Grouping " ^ expr_to_s m ^ ")"
  | Literal f ->
      "(Literal " ^ Float.to_string f ^ ")"
  | Unary (t, r) ->
      "(Unary " ^ Lex.lex_token_to_s t ^ " " ^ expr_to_s r ^ ")"

let%test _ = (parse_r (Literal 99.) [Lex.False]) = Literal(0.)
