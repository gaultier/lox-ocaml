open Base.Result

let read_whole_file filename =
  try
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch ; Ok s
  with Sys_error e -> fail [e]

let print_errors = List.iter prerr_endline

let lox_run filename =
  read_whole_file filename >>= Lox.Lex.lex >>= Lox.Parse.parse
  >>= Lox.Interpret.interpret Lox.Interpret.StringMap.empty
  |> iter_error ~f:print_errors

let rec repl env =
  Printf.printf "> " ;
  let env =
    (try read_line () with End_of_file -> exit 0)
    |> Lox.Lex.lex >>= Lox.Parse.parse
    >>= Lox.Interpret.interpret env
    >>| (fun (stmts, env) ->
          Base.Array.iter
            ~f:(fun s -> s |> Lox.Parse.value_to_string |> print_endline)
            stmts ;
          env)
    |> map_error ~f:print_errors |> Result.value ~default:env
  in
  repl env

let main () =
  match Sys.argv with
  | [|_; "repl"|] ->
      repl Lox.Interpret.StringMap.empty
  | [|_; "run"; filename|] ->
      lox_run filename
  | _ ->
      prerr_endline
        "Bad CLI invocation.\n\
         Use: `lox run foo.lox` to execute a file.\n\
         Use `lox repl` or `rlwrap lox repl` to launch the repl."

;;
main ()
