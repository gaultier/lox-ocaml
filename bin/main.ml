;;
"\"hey\" == \"hey\"" |> Lox.Lex.lex
|> List.iter (fun t ->
       Lox.Lex.sexp_of_lex_token t
       |> Base.Sexp.to_string_hum |> Printf.printf "%s ")

;;
print_endline ""

;;
"(1+ 3*4) == (15 - 2 * 1 -0)" |> Lox.Lex.lex |> Lox.Parse.expression |> fst
|> Lox.Parse.sexp_of_expr |> Base.Sexp.to_string_hum |> print_endline

;;
print_endline ""

;;
"\"hey\" == \"hey\"" |> Lox.Lex.lex |> Lox.Parse.expression |> fst
|> Lox.Interpret.eval |> Lox.Parse.sexp_of_literal_value |> Base.Sexp.to_string_hum
|> print_endline
