open Parse
open Base

module Expr = struct
  module T = struct
    type t = expr [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

type scope = (string, bool) Hashtbl.t

type scopes = scope Stack.t

type resolution = (expr, int) Hashtbl.t

let new_scope () : scope = Hashtbl.create (module String)

let print_resolution =
  Hashtbl.iteri ~f:(fun ~key:k ~data:d ->
      Stdlib.Printf.printf "- %s: %d\n" k d)

let declare_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:false)

let define_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:true)

let resolve_local resolution scopes expr n =
  let depth =
    Stack.fold_until ~init:0
      ~f:(fun depth scope ->
        match Hashtbl.find scope n with
        | Some _ ->
            Stop depth
        | None ->
            Continue depth)
      scopes
  in
  Hashtbl.set ~key:expr ~data:depth resolution

let rec var_resolve_scope scopes = function
  | Block stmts ->
      Stack.push scopes (new_scope ()) ;
      Array.iter ~f:(var_resolve_scope scopes) stmts ;
      Stack.pop_exn scopes |> ignore
  | Var (Lex.Identifier n, _) ->
      declare_var scopes n
  | _ ->
      ()

let resolve stmts =
  let scopes : scopes = Stack.create () in
  let resolution : resolution = Hashtbl.create (module Expr) in
  List.iter ~f:(fun stmt -> var_resolve_scope scopes stmt) stmts ;
  resolution

let var_resolve_expr resolution scopes = function
  | Variable (Lex.Identifier n) as v ->
      Stack.top scopes
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope n)
      |> Option.iter ~f:(fun b ->
             if !b then
               Printf.failwithf
                 "Cannot read local variable `%s` in its own initializer" n ()) ;
      resolve_local resolution scopes v n
  | _ ->
      ()
