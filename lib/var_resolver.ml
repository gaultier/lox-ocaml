open Parse
open Base

type scope = (string, bool) Hashtbl.t

type scopes = scope Stack.t

let new_scope () : scope = Hashtbl.create (module String)

let declare_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.add_exn scope ~key:name ~data:false)

let define_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.add_exn scope ~key:name ~data:true)

let rec var_resolve scopes = function
  | Block stmts ->
      Stack.push scopes (new_scope ()) ;
      Array.iter ~f:(var_resolve scopes) stmts ;
      Stack.pop_exn scopes |> ignore
  | Var (Lex.Identifier n, _) ->
      declare_var scopes n
  | _ ->
      ()
