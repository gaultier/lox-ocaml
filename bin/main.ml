;;
let tokens =
  Lox.Lex.lex "and 123.4 or (){},.-+;*/@!.!====<=<>>=// abc\n!\"hey\"!\"a!"
in
tokens |> List.iter (fun t -> Printf.printf "%s\n" (Lox.Lex.lex_token_to_s t))

open Lox.Parser

let e = Binary (Unary (Minus, Literal 1.), Plus, Unary (Plus, Literal 2.))

;;
print_endline (Lox.Parser.expr_to_s e)
