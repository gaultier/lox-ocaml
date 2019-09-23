;;
let tokens =
  Lox.Lex.lex "and 123.4 or (){},.-+;*/@!.!====<=<>>=// abc\n!\"hey\"!\"a!"
in
tokens |> List.iter (fun t -> Printf.printf "%s\n" (Lox.Lex.lex_token_to_s t))

;;
"-1 + 2 / -3 >= 5 == 6" |> Lox.Lex.lex |> Lox.Parse.expression |> fst
|> Lox.Parse.expr_to_s |> print_endline

;;
"-1 + 3" |> Lox.Lex.lex |> Lox.Parse.expression |> fst |> Lox.Interpret.eval
|> Lox.Parse.literal_to_s |> print_endline
