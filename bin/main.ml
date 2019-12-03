open Base
open Base.Result.Monad_infix

let ( >> ) f g x = g (f x)

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
  >>= Lox.Var_resolver.resolve (Map.empty (module Int))
  >>= (fun (stmts, resolution) ->
        Lox.Interpret.interpret resolution Lox.Parse.globals stmts)
  |> Result.iter_error ~f:print_errors

let rec repl resolution =
  Stdlib.Printf.printf "> ";
  let resolution =
    (try Stdlib.read_line () with End_of_file -> Stdlib.exit 0)
    |> Lox.Lex.lex >>= Lox.Parse.parse
    >>= Lox.Var_resolver.resolve resolution
    >>= (fun (stmts, resolution) ->
          Lox.Interpret.interpret resolution Lox.Parse.globals stmts
          >>| fun stmts -> (stmts, resolution))
    >>| (fun (stmts, resolution) ->
          Array.iter
            ~f:(fun s -> s |> Lox.Parse.value_to_string |> Stdlib.print_endline)
            stmts;
          resolution)
    |> Result.map_error ~f:print_errors
    |> Result.ok
    |> Option.value ~default:resolution
  in
  repl resolution

let dump_ast =
  Lox.Parse.sexp_of_statements >> Sexp.to_string_hum >> Stdlib.print_endline

let main () =
  match Sys.argv with
  | [| _; "repl" |] -> repl (Map.empty (module Int))
  | [| _; "dump"; "ast" |] ->
      read_from_stdin () >>= Lox.Lex.lex >>= Lox.Parse.parse
      |> Result.map_error ~f:print_errors
      |> Result.iter ~f:dump_ast
  | [| _; "dump"; "ast"; filename |] ->
      filename |> read_whole_file >>= Lox.Lex.lex >>= Lox.Parse.parse
      |> Result.map_error ~f:print_errors
      |> Result.iter ~f:dump_ast
  | [| _; "dump"; "resolution"; filename |] ->
      filename |> read_whole_file >>= Lox.Lex.lex >>= Lox.Parse.parse
      >>= Lox.Var_resolver.resolve (Map.empty (module Int))
      |> Result.map_error ~f:print_errors
      |> Result.iter ~f:(fun (_, resolution) ->
             Lox.Var_resolver.print_resolution resolution)
  | [| _; "dump"; "resolution" |] ->
      read_from_stdin () >>= Lox.Lex.lex >>= Lox.Parse.parse
      >>= Lox.Var_resolver.resolve (Map.empty (module Int))
      |> Result.map_error ~f:print_errors
      |> Result.iter ~f:(fun (_, resolution) ->
             Lox.Var_resolver.print_resolution resolution)
  | [| _; "run" |] -> read_from_stdin () |> lox_run
  | [| _; "run"; filename |] -> filename |> read_whole_file |> lox_run
  | [| _; "help" |] | _ ->
      Stdlib.print_endline
        ( "Use: `lox run foo.lox` to execute a file.\n\
           Use: `printf 'print 2*3;' | lox run` to read and execute from stdin.\n\
           Use: `lox repl` or `rlwrap lox repl` to launch the repl.\n\
           Use: `lox dump ast` to print the AST without running the program.\n\
           Use: `lox dump resolution` to print the variable resolution result \
           without running the program.\n\
           Use: `lox help` to see this help message.\n\
           CLI invocation was:"
        ^ Array.fold ~f:(fun acc s -> acc ^ " " ^ s) ~init:"" Sys.argv )

;;
main ()
