let rec repl () =
  Printf.printf "> " ;
  read_line () |> Lox.Lex.lex |> Lox.Parse.parse |> Lox.Interpret.eval
  |> Lox.Parse.sexp_of_literal_value |> Base.Sexp.to_string_hum
  |> print_endline ;
  repl ()

;;
repl ()
