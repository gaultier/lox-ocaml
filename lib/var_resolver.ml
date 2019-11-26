open Parse
open Base

type scope = (string, bool) Hashtbl.t [@@deriving sexp_of]


type scopes = scope Stack.t
[@@deriving sexp_of]

module Expr = struct
  module T = struct
    type t = expr

    let compare = compare_expr

    let sexp_of_t = sexp_of_expr
  end

  include T
  include Comparator.Make (T)
end

type resolution = (Expr.t, int, Expr.comparator_witness) Map.t

let new_scope () : scope = Hashtbl.create (module String)

let print_resolution (resolution : resolution) =
  Map.iteri
    ~f:(fun ~key:k ~data:d ->
      Stdlib.Printf.printf "- %s: %d\n" (k |> sexp_of_expr |> Sexp.to_string_hum) d)
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
  Map.set resolution ~key:expr ~data:depth

let rec resolve_function (resolution : resolution) (scopes : scopes) = function
    | Function(_, args, stmts) ->
  Stack.push scopes (new_scope ());
  List.iter ~f:(fun arg -> match arg with {kind= Identifier n; _} -> declare_var scopes n ; define_var scopes n;  | _ -> failwith "Malformed function argument") args;
  let resolution = resolve_stmts resolution scopes stmts in 
  Stack.pop_exn scopes |> ignore;
  resolution
    | _ -> failwith "Malformed function declaration"

and resolve_expr (resolution : resolution) (scopes : scopes) = function
  | Assign (Lex.Identifier n, expr) as assignment ->
      let resolution = resolve_expr resolution scopes expr in
      resolve_local resolution scopes assignment n
  | Variable (Lex.Identifier n) as v ->
      Stack.top scopes
      |> Option.bind ~f:(fun scope -> Hashtbl.find scope n)
      |> Option.iter ~f:(fun b ->
             if Bool.equal b false then
               Printf.failwithf
                 "Cannot read local variable `%s` in its own initializer" n ());
      resolve_local resolution scopes v n
  | Call(callee, _, args) -> 
          let resolution = resolve_expr resolution scopes callee in 
          let resolution = List.fold ~init:resolution ~f:(fun resolution arg -> resolve_expr resolution scopes arg) args in
          resolution
  | Binary (left, _, right) | LogicalOr(left, right) | LogicalAnd(left, right) -> let resolution = resolve_expr resolution scopes left in resolve_expr resolution scopes right 
  | Unary (_, e) | Grouping e -> resolve_expr resolution scopes e
  | Literal _ -> resolution
  | Assign (_) | Variable(_) -> failwith "Malformed AST node"

and resolve_stmt (resolution : resolution) (scopes : scopes) = function
  | Block stmts ->
      Stack.push scopes (new_scope ());
      let resolution =
        Array.fold
          ~f:(fun resolution stmt -> resolve_stmt resolution scopes stmt)
          ~init:resolution stmts
      in
      Stack.pop_exn scopes |> ignore;
      resolution
  | Var (Lex.Identifier n, expr) ->
      declare_var scopes n;
      let resolution = resolve_expr resolution scopes expr in
      define_var scopes n;
      resolution
  | Print e | Expr e | Return (_, e) -> resolve_expr resolution scopes e
  | Function ({ Lex.kind = Lex.Identifier name; _ }, _, _) as fn ->
      declare_var scopes name;
      define_var scopes name;
      resolve_function resolution scopes fn
  | WhileStmt (e, stmt) | IfStmt(e, stmt) -> let resolution = resolve_expr resolution scopes e in  resolve_stmt resolution scopes stmt 
  | IfElseStmt(e, then_stmt, else_stmt) ->  let resolution = resolve_expr resolution scopes e in let resolution = resolve_stmt resolution scopes then_stmt in resolve_stmt resolution scopes else_stmt

  | Var(_) | Function(_) -> failwith "Malformed AST node" 

and resolve_stmts (resolution: resolution) (scopes: scopes) (stmts: statement list) =
   List.fold
    ~f:(fun resolution stmt -> resolve_stmt resolution scopes stmt)
    ~init:resolution stmts

let resolve stmts = 
  let resolution : resolution = Map.empty (module Expr) in
  let scopes : scopes = Stack.create () in
  Stack.push scopes (new_scope ());
  let resolution = resolve_stmts resolution scopes stmts in 
   Stack.pop_exn scopes |> ignore;
   resolution
