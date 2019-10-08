
let read_lines name=
let buf = Buffer.create 0 in
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s ->  Buffer.add_string acc s;  loop acc
    | None -> close_in ic; acc
  loop buf

let loxc env =
  
  let stmts, env =
  read_lines "./hello_world.lox" |> Base.Buffer.To_string (String)  |> Lox.Lex.lex |> Lox.Parse.parse
    |> Lox.Interpret.interpret env
  in
  Array.iter Lox.Interpret.print stmts ;
  repl env

let rec repl env =
  Printf.printf "> " ;
  let stmts, env =
    read_line () |> Lox.Lex.lex |> Lox.Parse.parse
    |> Lox.Interpret.interpret env
  in
  Array.iter Lox.Interpret.print stmts ;
  repl env

;;
repl Lox.Interpret.StringMap.empty
