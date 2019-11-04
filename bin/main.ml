open Base

let ( >>| ) = Result.( >>| )

let ( >>= ) = Result.( >>= )

let read_whole_file filename =
  try
    let ch = Stdlib.open_in filename in
    let s = Stdlib.really_input_string ch (Stdlib.in_channel_length ch) in
    Stdlib.close_in ch ; Ok s
  with Sys_error e -> Result.fail [e]

let print_errors = List.iter ~f:Stdlib.prerr_endline

let lox_run filename =
  read_whole_file filename >>= Lox.Lex.lex >>= Lox.Parse.parse
  >>= Lox.Interpret.interpret
        {Lox.Parse.values= Lox.Parse.globals; Lox.Parse.enclosing= None}
  |> Result.iter_error ~f:print_errors

let rec repl env =
  Stdlib.Printf.printf "> " ;
  let env =
    (try Stdlib.read_line () with End_of_file -> Stdlib.exit 0)
    |> Lox.Lex.lex >>= Lox.Parse.parse
    >>= Lox.Interpret.interpret env
    >>| (fun (stmts, env) ->
          Array.iter
            ~f:(fun s ->
              s |> Lox.Parse.value_to_string |> Stdlib.print_endline)
            stmts ;
          env)
    |> Result.map_error ~f:print_errors
    |> Result.ok |> Option.value ~default:env
  in
  repl env

let main () =
  match Sys.argv with
  | [|_; "repl"|] ->
      repl {Lox.Parse.values= Lox.Parse.globals; Lox.Parse.enclosing= None}
  | [|_; "run"; filename|] ->
      lox_run filename
  | _ ->
      Stdlib.prerr_endline
        "Bad CLI invocation.\n\
         Use: `lox run foo.lox` to execute a file.\n\
         Use `lox repl` or `rlwrap lox repl` to launch the repl."

;;
main ()
