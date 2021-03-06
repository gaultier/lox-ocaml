open Lex
open Base
open Base.Result.Let_syntax

type id = int [@@deriving compare, sexp_of]

let id : id ref = ref 0

let next_id () =
  id := !id + 1;
  !id

type function_signature = (value list -> environment -> value[@sexp.ignore])

and callable = {
  arity : int;
  name : string;
  is_ctor : bool;
  mutable decl_environment : (environment[@sexp.ignore]);
  fn : (function_signature[@sexp.ignore]);
}

and env_values_t = (string, value) Hashtbl.t

and environment = {
  values : (env_values_t[@sexp.ignore]);
  enclosing : environment option;
}

and methods = (string, value) Hashtbl.t

and value =
  | Bool of bool
  | Number of float
  | Nil
  | String of string
  | Callable of callable
  | VClass of string * value option * methods
  | Instance of value * env_values_t
[@@deriving sexp_of]

and expr =
  | Binary of expr * token_kind * expr * id
  | Grouping of expr * id
  | Literal of value * id
  | Unary of token_kind * expr * id
  | Assign of token_kind * expr * id
  | Variable of token_kind * id
  | LogicalOr of expr * expr * id
  | LogicalAnd of expr * expr * id
  | Call of expr * token * expr list * id
  | Get of expr * string
  | Set of expr * string * expr
  | This of token * id
  | Super of token * token * id
[@@deriving sexp_of]

and statement =
  | Expr of expr * id
  | Print of expr * id
  | Var of token_kind * expr * id
  | Block of statement array * id
  | Function of token * token list * statement list * id
  | Class of string * expr option * statement list * id
  | Return of token * expr * id
  | IfStmt of expr * statement * id
  | IfElseStmt of expr * statement * statement * id
  | WhileStmt of expr * statement * id
[@@deriving sexp_of]

and statements = statement list [@@deriving sexp_of]

let empty () : env_values_t = Hashtbl.create (module String)

let value_to_string = function
  | String s -> s
  | Number f -> Float.to_string f
  | Bool true -> "true"
  | Bool false -> "false"
  | Nil -> "nil"
  | Callable { name = n; _ } -> "function@" ^ n
  | VClass (n, _, _) -> n
  | Instance (VClass (n, _, _), _) -> "instance@" ^ n
  | Instance _ -> failwith "Malformed instance"

let globals : environment =
  {
    values =
      Hashtbl.of_alist_exn
        (module String)
        [
          ( "clock",
            Callable
              {
                arity = 0;
                name = "clock";
                is_ctor = false;
                decl_environment = { values = empty (); enclosing = None };
                fn = (fun _ _ -> Number (Unix.gettimeofday ()));
              } );
          ( "readLine",
            Callable
              {
                arity = 0;
                name = "readLine";
                is_ctor = false;
                decl_environment = { values = empty (); enclosing = None };
                fn =
                  (fun _ _ ->
                    String
                      (Stdio.In_channel.input_line_exn ~fix_win_eol:true
                         Stdio.stdin));
              } );
          ( "parseNumber",
            Callable
              {
                arity = 1;
                name = "parseNumber";
                is_ctor = false;
                decl_environment = { values = empty (); enclosing = None };
                fn =
                  (fun args _ ->
                    match List.hd_exn args with
                    | Number _ as n -> n
                    | String s -> Number (Float.of_string s)
                    | v ->
                        Printf.failwithf "Cannot convert `%s` to number"
                          (value_to_string v) ());
              } );
        ];
    enclosing = None;
  }

let rec sync acc = function
  (* Should we sync at other boundaries, e.g keywords here as well? *)
  | ({ kind = SemiColon; _ } as t) :: rest -> (t :: acc, rest)
  | [] -> (acc, [])
  | x :: r -> sync (x :: acc) r

let error ctx expected rest =
  let lines, columns =
    List.hd rest
    |> Option.map ~f:(fun { lines; columns; _ } -> (lines, columns))
    (*FIXME*)
    |> Option.value ~default:(1, 1)
  in
  let invalid, rest = sync [] rest in
  let invalid_s =
    match invalid with
    | [] -> "no more tokens"
    | _ :: _ ->
        invalid
        |> List.rev_map ~f:(fun { kind = x; _ } -> token_to_string x)
        |> List.fold ~init:"" ~f:(fun acc x -> acc ^ " " ^ x)
  in
  Result.fail
    ( Printf.sprintf "%d:%d:Context: %s. %s. Got: `%s`." lines columns ctx
        expected invalid_s,
      rest )

let rec primary = function
  | { kind = False; _ } :: rest -> Ok (Literal (Bool false, next_id ()), rest)
  | { kind = True; _ } :: rest -> Ok (Literal (Bool true, next_id ()), rest)
  | { kind = Nil; _ } :: rest -> Ok (Literal (Nil, next_id ()), rest)
  | { kind = Number f; _ } :: rest -> Ok (Literal (Number f, next_id ()), rest)
  | { kind = String s; _ } :: rest -> Ok (Literal (String s, next_id ()), rest)
  | { kind = ParenLeft; _ } :: rest ->
      let%bind e, rest = expression rest in
      let%bind _, rest = expect ParenRight rest in
      Ok (Grouping (e, next_id ()), rest)
  | ({ kind = This; _ } as t) :: rest -> Ok (This (t, next_id ()), rest)
  | ({ kind = Super; _ } as super)
    :: { kind = Dot; _ } :: ({ kind = Identifier _; _ } as m) :: rest ->
      Ok (Super (super, m, next_id ()), rest)
  | { kind = Super; _ } :: rest ->
      error "Super" "Expected `super` method call (e.g `super.foo()`)" rest
  | { kind = Identifier _ as i; _ } :: rest ->
      Ok (Variable (i, next_id ()), rest)
  | _ as rest ->
      error "Primary" "Expected primary (e.g `1` or `(true)` or \"hello\")" rest

and expect (kind : token_kind) = function
  | { kind = k; _ } :: rest when Stdlib.( = ) k kind -> Ok ((), rest)
  | rest ->
      error "" (Printf.sprintf "Expected `%s`" (token_to_string kind)) rest

and fn_call tokens =
  let%bind prim, rest = primary tokens in

  let rec fn_call_rec acc rest =
    match rest with
    | { kind = Dot; _ } :: { kind = Identifier n; _ } :: rest ->
        let acc = Get (acc, n) in
        fn_call_rec acc rest
    | { kind = Dot; _ } :: rest ->
        error "Property access" "Expected valid property access (e.g `foo.bar`)"
          rest
    | { kind = ParenLeft; _ } :: rest ->
        let%bind acc, rest = fn_call_arguments acc [] rest in
        fn_call_rec acc rest
    | _ -> Ok (acc, rest)
  in
  fn_call_rec prim rest

and fn_call_comma_argument = function
  | { kind = Comma; _ } :: rest -> expression rest
  | _ as rest ->
      error "Function call arguments" "Expected `,` before argument" rest

and fn_call_comma_arguments args = function
  | ({ kind = ParenRight; _ } as t) :: rest -> Ok (args, t, rest)
  | _ as rest ->
      let%bind arg, rest = fn_call_comma_argument rest in
      fn_call_comma_arguments (arg :: args) rest

and fn_call_arguments callee args = function
  | ({ kind = ParenRight; _ } as t) :: rest ->
      Ok (Call (callee, t, args, next_id ()), rest)
  | _ as rest ->
      let%bind expr, rest = expression rest in
      let%bind args, closing_paren, rest =
        fn_call_comma_arguments [ expr ] rest
      in
      let len = List.length args in
      if len >= 255 then
        Stdlib.prerr_endline
          (Printf.sprintf
             "Function call: Too many arguments: limit is 255, got: %d" len);
      Ok (Call (callee, closing_paren, List.rev args, next_id ()), rest)

and unary = function
  | { kind = (Bang | Minus) as t; _ } :: rest ->
      let%map right, rest = unary rest in
      (Unary (t, right, next_id ()), rest)
  | _ as t -> fn_call t

and multiplication tokens =
  let%bind left, rest = unary tokens in
  match rest with
  | { kind = (Star | Slash) as t; _ } :: rest ->
      let%map right, rest = multiplication rest in
      (Binary (left, t, right, next_id ()), rest)
  | _ -> Ok (left, rest)

and addition tokens =
  let%bind left, rest = multiplication tokens in
  match rest with
  | { kind = (Plus | Minus) as t; _ } :: rest ->
      let%map right, rest = addition rest in
      (Binary (left, t, right, next_id ()), rest)
  | _ -> Ok (left, rest)

and comparison tokens =
  let%bind left, rest = addition tokens in
  match rest with
  | { kind = (Greater | GreaterEqual | Less | LessEqual) as t; _ } :: rest ->
      let%map right, rest = comparison rest in
      (Binary (left, t, right, next_id ()), rest)
  | _ -> Ok (left, rest)

and equality tokens =
  let%bind left, rest = comparison tokens in
  match rest with
  | { kind = (BangEqual | EqualEqual) as t; _ } :: rest ->
      let%map right, rest = equality rest in
      (Binary (left, t, right, next_id ()), rest)
  | _ -> Ok (left, rest)

and expression tokens = assignment tokens

and assignment = function
  | [] as rest -> error "Assignement" "Expected assignement (e.g `a = 1;`)" rest
  | _ as t -> (
      let%bind lhs, rest = logic_or t in
      match rest with
      | { kind = Equal; _ } :: rest -> (
          let%bind rhs, rest = assignment rest in
          match (lhs, rhs) with
          | Variable (v, _), _ -> Ok (Assign (v, rhs, next_id ()), rest)
          | Get (lhs, n), _ -> Ok (Set (lhs, n, rhs), rest)
          | _ -> error "Assignement" "Expected valid assignment target" t )
      | _ -> Ok (lhs, rest) )

and logic_and tokens =
  let%bind l, rest = equality tokens in
  match rest with
  | { kind = And; _ } :: rest ->
      let%map r, rest = logic_and rest in
      (LogicalAnd (l, r, next_id ()), rest)
  | _ -> Ok (l, rest)

and logic_or tokens =
  let%bind l, rest = logic_and tokens in
  match rest with
  | { kind = Or; _ } :: rest ->
      let%map r, rest = logic_or rest in
      (LogicalOr (l, r, next_id ()), rest)
  | _ -> Ok (l, rest)

and expression_stmt = function
  | [] as rest ->
      error "Expression statement" "Expected expression statement (e.g `1;`)"
        rest
  | _ as t ->
      let%bind stmt, rest = expression t in
      let%bind _, rest = expect SemiColon rest in
      Ok (stmt, rest)

and print_stmt = function
  | { kind = Print; _ } :: rest ->
      let%map expr, rest = expression_stmt rest in
      (Print (expr, next_id ()), rest)
  | _ as rest ->
      error "Print statement" "Expected print statement (e.g `print 1;`)" rest

and if_stmt = function
  | { kind = If; _ } :: { kind = ParenLeft; _ } :: rest -> (
      let%bind e, rest = expression rest in
      match rest with
      | { kind = ParenRight; _ } :: rest -> (
          let%bind then_stmt, rest = statement rest in
          match rest with
          | { kind = Else; _ } :: rest ->
              let%map else_stmt, rest = statement rest in
              (IfElseStmt (e, then_stmt, else_stmt, next_id ()), rest)
          | _ -> Ok (IfStmt (e, then_stmt, next_id ()), rest) )
      | _ -> error "If statement" "Expected closing `)`" rest )
  | _ as rest ->
      error "If statement" "Expected if statement (e.g `if (true) print 3;`)"
        rest

and while_stmt = function
  | { kind = While; _ } :: { kind = ParenLeft; _ } :: rest ->
      let%bind e, rest = expression rest in
      let%bind _, rest = expect ParenRight rest in
      let%map s, rest = statement rest in
      (WhileStmt (e, s, next_id ()), rest)
  | _ as rest ->
      error "While statement" "Expected while statement: (e.g `while(true) {}`)"
        rest

and for_stmt = function
  (* for (;;) *)
  (* for (;i; i = i+1) *)
  (* for (;i;) *)
  (* for (i;;i=i+1) *)
  (* for (;;i=i+1)  *)
  (* for (var i = 0; i < 5; i = i + 1) *)
  | { kind = For; _ } :: { kind = ParenLeft; _ } :: rest ->
      let%bind init_clause, rest =
        match rest with
        | { kind = Var; _ } :: _ -> var_decl rest
        | { kind = SemiColon; _ } :: rest ->
            Ok (Expr (Literal (Bool true, next_id ()), next_id ()), rest)
        | _ ->
            let%map e, rest = expression_stmt rest in
            (Expr (e, next_id ()), rest)
      in
      let%bind stop_cond, rest =
        match rest with
        | { kind = SemiColon; _ } :: rest ->
            Ok (Literal (Bool true, next_id ()), rest)
        | _ ->
            let%bind e, rest = expression rest in
            let%bind _, rest = expect SemiColon rest in
            Ok (e, rest)
      in
      let%bind incr_stmt, rest =
        match rest with
        | { kind = ParenRight; _ } :: _ ->
            Ok (Literal (Bool true, next_id ()), rest)
        | _ -> expression rest
      in
      let%bind _, rest = expect ParenRight rest in
      let%bind body, rest = statement rest in
      let%bind enclosed_body =
        Ok
          (Block
             ( [|
                 init_clause;
                 WhileStmt
                   ( stop_cond,
                     Block ([| body; Expr (incr_stmt, next_id ()) |], next_id ()),
                     next_id () );
               |],
               next_id () ))
      in
      Ok (enclosed_body, rest)
  | _ as rest -> error "For-loop" "Expected loop (e.g `for (;;)`)" rest

and block_stmt_inner tokens (acc : (statement, string) Base.Result.t list) =
  match tokens with
  | [] -> error "Block statement" "Expected closing `}`" tokens
  | { kind = CurlyBraceRight; _ } :: rest ->
      let acc : (statement list, string list) Base.Result.t =
        acc |> List.rev |> Result.combine_errors
      in
      acc
      |> Result.map ~f:(fun stmts -> (stmts, rest))
      |> Result.map_error ~f:(fun errs ->
             (List.fold ~f:(fun acc err -> acc ^ err ^ "\n") ~init:"" errs, rest))
  | _ ->
      let d = declaration tokens in
      let rest : token list =
        match d with Ok (_, rest) | Error (_, rest) -> rest
      in
      let ok_or_err : (statement, string) Base.Result.t =
        Result.map ~f:fst d |> Result.map_error ~f:fst
      in

      let acc = ok_or_err :: acc in
      block_stmt_inner rest acc

and block_stmt = function
  | { kind = CurlyBraceLeft; _ } :: rest ->
      let%map stmts, rest = block_stmt_inner rest [] in
      (Block (Array.of_list stmts, next_id ()), rest)
  | _ as rest ->
      error "Block statement" "Expected block statement with opening `{`" rest

and return_stmt = function
  | ({ kind = Return; _ } as ret) :: { kind = SemiColon; _ } :: rest ->
      Ok (Return (ret, Literal (Nil, next_id ()), next_id ()), rest)
  | ({ kind = Return; _ } as ret) :: rest ->
      let%bind e, rest = expression rest in
      let%bind _, rest = expect SemiColon rest in
      Ok (Return (ret, e, next_id ()), rest)
  | _ as rest ->
      error "Return statement" "Expected return statement (e.g `return 1+2;`)"
        rest

and statement = function
  | [] as rest -> error "Statement" "Expected statement (e.g `x = 1;`)" rest
  | { kind = Print; _ } :: _ as t -> print_stmt t
  | { kind = CurlyBraceLeft; _ } :: _ as t -> block_stmt t
  | { kind = If; _ } :: _ as t -> if_stmt t
  | { kind = While; _ } :: _ as t -> while_stmt t
  | { kind = For; _ } :: _ as t -> for_stmt t
  | { kind = Return; _ } :: _ as t -> return_stmt t
  | _ as t ->
      let%map e, rest = expression_stmt t in
      (Expr (e, next_id ()), rest)

and var_decl = function
  | { kind = Var; _ }
    :: { kind = Identifier n; _ } :: { kind = Equal; _ } :: rest ->
      let%bind e, rest = expression rest in
      let%bind _, rest = expect SemiColon rest in
      Ok (Var (Identifier n, e, next_id ()), rest)
  | { kind = Var; _ }
    :: { kind = Identifier n; _ } :: { kind = SemiColon; _ } :: rest ->
      Ok (Var (Identifier n, Literal (Nil, next_id ()), next_id ()), rest)
  | _ as rest ->
      error "Variable declaration"
        "Expected variable declaration (e.g `var x = 1;`)" rest

and function_decl = function
  | ({ kind = Identifier _; _ } as name) :: { kind = ParenLeft; _ } :: rest -> (
      let%bind args, rest = fn_decl_arguments [] rest in
      let%bind block, rest = block_stmt rest in
      match block with
      | Block (statements, _) ->
          Ok (Function (name, args, Array.to_list statements, next_id ()), rest)
      | _ ->
          error "Function declaration"
            "Expected function body (e.g `{ print 1; print 2;}`)" rest )
  | _ as rest ->
      error "Function declaration"
        "Expected function declaration (e.g `fun foo {print 1;}`)" rest

and fn_decl_comma_argument = function
  | { kind = Comma; _ } :: ({ kind = Identifier _; _ } as identifier) :: rest ->
      Ok (identifier, rest)
  | { kind = Comma; _ } :: { kind = ParenRight; _ } :: _ as rest ->
      error "Function declaration"
        "Trailing commas are not allowed in function arguments" rest
  | _ as rest ->
      error "Function declaration" "Expected `,` before argument" rest

and fn_decl_comma_arguments args = function
  | { kind = ParenRight; _ } :: rest -> Ok (args, rest)
  | _ as rest ->
      let%bind arg, rest = fn_decl_comma_argument rest in
      fn_decl_comma_arguments (arg :: args) rest

and fn_decl_arguments args = function
  | { kind = ParenRight; _ } :: rest -> Ok (List.rev args, rest)
  | ({ kind = Identifier _; _ } as identifier) :: rest ->
      let%bind args, rest = fn_decl_comma_arguments [ identifier ] rest in
      let len = List.length args in
      if len >= 255 then
        Stdlib.prerr_endline
          (Printf.sprintf
             "Function definition: Too many arguments: limit is 255, got: %d"
             len);
      Ok (List.rev args, rest)
  | _ as rest ->
      error "Function definition" "Expected argument list (e.g `(a, b)`)" rest

and methods_decl acc = function
  | { kind = CurlyBraceRight; _ } :: rest -> Ok (List.rev acc, rest)
  | rest ->
      let%bind fn, rest = function_decl rest in
      methods_decl (fn :: acc) rest

and class_decl = function
  | { kind = Class; _ }
    :: { kind = Identifier n; _ } :: { kind = CurlyBraceLeft; _ } :: rest ->
      let%map methods, rest = methods_decl [] rest in
      (Class (n, None, methods, next_id ()), rest)
  | { kind = Class; _ }
    :: { kind = Identifier n; _ } :: { kind = Less; _ } :: rest ->
      let%bind superclass, rest = primary rest in
      let%bind _, rest = expect CurlyBraceLeft rest in
      let%map methods, rest = methods_decl [] rest in
      (Class (n, Some superclass, methods, next_id ()), rest)
  | rest ->
      error "Class declaration"
        "Expected valid class declaration, e.g (`class foo {}`)" rest

and declaration d =
  match d with
  | { kind = Var; _ } :: _ -> var_decl d
  | { kind = Fun; _ } :: rest -> function_decl rest
  | { kind = Class; _ } :: _ -> class_decl d
  | _ -> statement d

and program decls = function
  | [] -> decls
  | _ as t ->
      let d = declaration t in
      let rest =
        match d with Ok (_, rest) -> rest | Error (_, rest) -> rest
      in
      let ok_or_err = Result.map ~f:fst d |> Result.map_error ~f:fst in
      let decls = ok_or_err :: decls in
      program decls rest

let parse tokens = program [] tokens |> List.rev |> Result.combine_errors
