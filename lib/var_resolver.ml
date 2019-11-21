open Parse
open Base

type scope = (string, bool) Hashtbl.t

type scopes = scope Stack.t

type resolution = (statement * int) list

let new_scope () : scope = Hashtbl.create (module String)

let print_resolution (resolution : resolution) =
  List.iter
    ~f:(fun (k, d) ->
      let n = match k with Var (Lex.Identifier n, _) -> n | _ -> "unkown" in
      Stdlib.Printf.printf "- %s: %d\n" n d)
    resolution

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
        | Some _ -> Stop depth
        | None -> Continue depth)
      scopes
  in
  (expr, depth) :: resolution

let rec var_resolve_scope scopes = function
  | Block stmts ->
      Stack.push scopes (new_scope ());
      Array.iter ~f:(var_resolve_scope scopes) stmts;
      Stack.pop_exn scopes |> ignore
  | Var (Lex.Identifier n, _) -> declare_var scopes n
  | _ -> ()

let resolve stmts =
  let scopes : scopes = Stack.create () in
  let resolution : resolution = [] in
  List.iter ~f:(fun stmt -> var_resolve_scope scopes stmt) stmts;
  resolution

let var_resolve_expr resolution scopes = function
  | Variable (Lex.Identifier n) as v ->
      Stack.top scopes
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope n)
      |> Option.iter ~f:(fun b ->
             if !b then
               Printf.failwithf
                 "Cannot read local variable `%s` in its own initializer" n ());
      resolve_local resolution scopes v n
  | _ -> resolution
