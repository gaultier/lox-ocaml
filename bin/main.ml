;;
let tokens = Lox.Lex.lex "{" in
List.iter (fun t -> print_string (Lox.Lex.lex_token_to_s t)) tokens
