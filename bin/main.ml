;;
let tokens = Lox.Lex.lex "(){},.-+;*/@" in
tokens |> List.rev
|> List.iter (fun t -> Printf.printf "%s\n" (Lox.Lex.lex_token_to_s t))
