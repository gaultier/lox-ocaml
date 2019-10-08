
let read_lines name=
let buf = Buffer.create 16 in
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop () = match try_read () with
    | Some s ->  Buffer.add_string buf s;  loop ();
    | None -> close_in ic ;
in
  loop (); buf

let loxc filename =
  read_lines filename|> Base.Buffer.contents |> Lox.Lex.lex |> Lox.Parse.parse
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
| [|_; "repl"|] -> repl Lox.Interpret.StringMap.empty
| [|_; "build"; filename|] -> loxc filename
| _ -> failwith "Bad CLI invocation"

;;
main ()
