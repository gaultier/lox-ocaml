open Parse
open Base

type scope = (string, bool) Hashtbl.t

type scopes = scope Stack.t

type resolution = (expr * int) list

let new_scope () : scope = Hashtbl.create (module String)

let print_resolution (resolution : resolution) =
  List.iter
    ~f:(fun (k, d) ->
      let n = match k with Variable (Lex.Identifier n) -> n | _ -> "unkown" in
      Stdlib.Printf.printf "- %s: %d\n" n d)
    resolution

let declare_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:false)

let define_var scopes name =
  Stack.top scopes
  |> Option.iter ~f:(fun scope -> Hashtbl.set scope ~key:name ~data:true)

let resolve_local (resolution : resolution) (scopes : scopes) expr n =
  let depth =
    Stack.fold_until ~init:0
      ~f:(fun depth scope ->
        match Hashtbl.find scope n with
        | Some _ -> Stop depth
        | None -> Continue (depth + 1))
      ~finish:(fun depth -> depth)
      scopes
  in
  (expr, depth) :: resolution

let var_resolve_expr (resolution : resolution) (scopes : scopes) = function
  | Variable (Lex.Identifier n) as v ->
      Stack.top scopes
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope n)
      |> Option.iter ~f:(fun b ->
             if Bool.equal b false then
               Printf.failwithf
                 "Cannot read local variable `%s` in its own initializer" n ());
      resolve_local resolution scopes v n
  | _ -> resolution

let rec var_resolve_scope (resolution : resolution) (scopes : scopes) = function
  | Block stmts ->
      Stack.push scopes (new_scope ());
      let resolution =
        Array.fold
          ~f:(fun resolution stmt -> var_resolve_scope resolution scopes stmt)
          ~init:resolution stmts
      in
      Stack.pop_exn scopes |> ignore;
      resolution
  | Var (Lex.Identifier n, expr) ->
      declare_var scopes n;
      let resolution = var_resolve_expr resolution scopes expr in
      define_var scopes n;
      resolution
  | Print e | Expr e -> var_resolve_expr resolution scopes e
  | _ -> resolution

let resolve stmts =
  let scopes : scopes = Stack.create () in
  Stack.push scopes (new_scope ());
  List.fold
    ~f:(fun resolution stmt -> var_resolve_scope resolution scopes stmt)
    ~init:[] stmts
