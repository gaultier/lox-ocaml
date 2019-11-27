open Base

let ( >>| ) = Result.( >>| )

let ( >>= ) = Result.( >>= )

let read_whole_file filename =
  try
    let ch = Stdlib.open_in filename in
    let s = Stdlib.really_input_string ch (Stdlib.in_channel_length ch) in
    Stdlib.close_in ch;
    Ok s
  with Sys_error e -> Result.fail [ e ]

let read_from_stdin () =
  let rec read acc =
    try
      let s = Stdlib.read_line () in
      read (acc ^ s)
    with _ -> acc
  in
  Ok (read "")

let print_errors = List.iter ~f:Stdlib.prerr_endline

let lox_run input =
  input >>= Lox.Lex.lex >>= Lox.Parse.parse
  >>| (fun stmts ->
        let resolution = Lox.Var_resolver.resolve stmts in
        (stmts, resolution))
  >>= (fun (stmts, resolution) ->
        Lox.Interpret.interpret resolution Lox.Parse.globals stmts)
  |> Result.iter_error ~f:print_errors

let rec repl env =
  Stdlib.Printf.printf "> ";
  let env =
    (try Stdlib.read_line () with End_of_file -> Stdlib.exit 0)
    |> Lox.Lex.lex >>= Lox.Parse.parse
    >>| (fun stmts ->
          let resolution = Lox.Var_resolver.resolve stmts in
          (stmts, resolution))
    >>= (fun (stmts, resolution) ->
          Lox.Interpret.interpret resolution Lox.Parse.globals stmts)
    >>| (fun stmts ->
          Array.iter
            ~f:(fun s -> s |> Lox.Parse.value_to_string |> Stdlib.print_endline)
            stmts;
          env)
    |> Result.map_error ~f:print_errors
    |> Result.ok |> Option.value ~default:env
  in
  repl env

let main () =
  match Sys.argv with
  | [| _; "repl" |] -> repl Lox.Parse.globals
  | [| _; "run"; "-" |] -> read_from_stdin () |> lox_run
  | [| _; "run"; filename |] -> filename |> read_whole_file |> lox_run
  | _ ->
      Stdlib.prerr_endline
        ( "Use: `lox run foo.lox` to execute a file.\n\
           Use: `printf 'print 2*3;' | lox run -` to read and execute from \
           stdin.\n\
           Use `lox repl` or `rlwrap lox repl` to launch the repl.\n\
           CLI invocation was: "
        ^ Array.fold ~f:(fun acc s -> acc ^ s) ~init:" " Sys.argv )

;;
main ()
