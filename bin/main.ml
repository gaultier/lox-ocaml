let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch ; s

let lox_run filename =
  read_whole_file filename |> Lox.Lex.lex |> Result.get_ok |> Lox.Parse.parse
  |> Lox.Interpret.interpret Lox.Interpret.StringMap.empty

let rec repl env =
  Printf.printf "> " ;
  let stmts, env =
    try
      read_line () |> Lox.Lex.lex |> Result.get_ok |> Lox.Parse.parse
      |> Lox.Interpret.interpret env
    with
    | Failure msg ->
        print_endline ("Error: " ^ msg) ;
        ([||], env)
    | End_of_file ->
        print_endline "Bye!" ; exit 0
  in
  Array.iter Lox.Interpret.print stmts ;
  repl env

let main () =
  match Sys.argv with
  | [|_; "repl"|] ->
      repl Lox.Interpret.StringMap.empty
  | [|_; "run"; filename|] ->
      lox_run filename
  | _ ->
      failwith "Bad CLI invocation"

;;
main ()
