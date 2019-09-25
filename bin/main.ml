;;
"(-1 + 3 * 5) == (2*5 + 4)" |> Lox.Lex.lex |> Lox.Parse.expression |> fst
|> Lox.Interpret.eval |> Lox.Parse.literal_to_s |> print_endline
