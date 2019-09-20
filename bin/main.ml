;;
let tokens =
  Lox.Lex.lex "and 123.4 or (){},.-+;*/@!.!====<=<>>=// abc\n!\"hey\"!\"a!"
in
tokens |> List.iter (fun t -> Printf.printf "%s\n" (Lox.Lex.lex_token_to_s t))
;;

"1 * 2 * -3" |> Lox.Lex.lex |> Lox.Parser.multiplication_r |> fst |> Lox.Parser.expr_to_s |> print_endline

