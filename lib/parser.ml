type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of float
  | Unary of Lex.lex_token * expr

let e = Binary (Unary (Minus, Literal 1.), Plus, Unary (Plus, Literal 2.))
