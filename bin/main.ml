module StringMap = Map.Make (String)

let rec repl env =
  Printf.printf "> " ;
  let stmts, env =
    read_line () |> Lox.Lex.lex |> Lox.Parse.parse
    |> Lox.Interpret.interpret env
  in
  List.iter Lox.Interpret.print stmts ;
  print_endline "" ;
  repl env

;;
repl StringMap.empty
