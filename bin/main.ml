;;
let tokens = Lox.Lex.lex "(){},.-+;*/@" in
List.iter (fun t -> Printf.printf "%s\n" (Lox.Lex.lex_token_to_s t)) tokens
