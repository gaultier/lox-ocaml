let rec repl () =
  Printf.printf "> " ;
   read_line () |> Lox.Lex.lex |> Lox.Parse.parse |> Lox.Interpret.interpret 
  |> List.iter (fun s -> Lox.Parse.sexp_of_literal_value s |> Base.Sexp.to_string_hum |> print_endline);
print_endline "";
  repl ()

;;
repl ()
