(* FIXME *)
let read_lines name =
  let buf = Bytes.create 200 in
  let ic = open_in name in
  let _ = input ic buf 0 200 in
  Bytes.to_string buf

let loxc filename =
  read_lines filename |> Lox.Lex.lex |> Lox.Parse.parse
  |> Lox.Interpret.interpret Lox.Interpret.StringMap.empty

let rec repl env =
  Printf.printf "> " ;
  let stmts, env =
    read_line () |> Lox.Lex.lex |> Lox.Parse.parse
    |> Lox.Interpret.interpret env
  in
  Array.iter Lox.Interpret.print stmts ;
  repl env

let main () =
  match Sys.argv with
  | [|_; "repl"|] ->
      repl Lox.Interpret.StringMap.empty
  | [|_; "build"; filename|] ->
      loxc filename
  | _ ->
      failwith "Bad CLI invocation"

;;
main ()
