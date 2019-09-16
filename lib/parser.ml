type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of float
  | Unary of Lex.lex_token * expr

let e = Binary (Unary (Minus, Literal 1.), Plus, Unary (Plus, Literal 2.))

let rec expr_to_s e = match e with 
| Binary (l, t, r) -> "(Binary " ^ expr_to_s l ^ Lex.lex_token_to_s t ^ expr_to_s r
| _ -> ""
