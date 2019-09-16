;;
let tokens =
  Lox.Lex.lex "and 123.4 or (){},.-+;*/@!.!====<=<>>=// abc\n!\"hey\"!\"a!"
in
tokens |> List.rev
|> List.iter (fun t -> Printf.printf "%s\n" (Lox.Lex.lex_token_to_s t))
