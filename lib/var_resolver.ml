open Parse
open Base

type scope = (string, bool) Hashtbl.t

type scopes = scope Stack.t

type local_var_resolution = (expr, int) Hashtbl.t

let new_scope () : scope = Hashtbl.create (module String)

let declare_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:false)

let define_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:true)

let resolve_local scopes expr n =
  Stack.fold_until ~init:0
    ~f:(fun depth scope ->
      match Hashtbl.find scope n with
      | Some _ ->
          Stop depth
      | None ->
          Continue depth)
    scopes

let rec var_resolve scopes = function
  | Block stmts ->
      Stack.push scopes (new_scope ()) ;
      Array.iter ~f:(var_resolve scopes) stmts ;
      Stack.pop_exn scopes |> ignore
  | Var (Lex.Identifier n, _) ->
      declare_var scopes n
  | _ ->
      ()

let var_resolve_expr scopes = function
  | Variable (Lex.Identifier n) ->
      Stack.top scopes
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope n)
      |> Option.iter ~f:(fun b ->
             if !b then
               Printf.failwithf
                 "Cannot read local variable `%s` in its own initializer" n ()) ;
      resolve_local scopes
  | _ ->
      ()
