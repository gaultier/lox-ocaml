;;
"\"hey\" == \"hey\"" |> Lox.Lex.lex
|> List.iter (fun t ->
       Lox.Lex.sexp_of_lex_token t
       |> Base.Sexp.to_string |> Printf.printf "%s ")

;;
print_endline ""

;;
"\"hey\" == \"hey\"" |> Lox.Lex.lex |> Lox.Parse.expression |> fst
|> Lox.Interpret.eval |> Lox.Parse.literal_to_s |> print_endline
