open Parse
open Base

type scope = (string, bool) Hashtbl.t

type scopes = scope Stack.t

let new_scope () : scope = Hashtbl.create (module String)

let var_resolve scopes = function
  | Block _ ->
      Stack.push scopes (new_scope ()) ;
      Stack.pop_exn scopes |> ignore
  | _ ->
      ()
