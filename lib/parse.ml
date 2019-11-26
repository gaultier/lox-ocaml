open Lex
open Base

let ( let* ) x f = Result.bind x ~f

let ( let+ ) x f = Result.map ~f x

type function_signature = (value list -> environment -> value[@ignore])

and callable = {
  arity : int;
  name : string;
  mutable decl_environment : (environment[@ignore]);
  fn : (function_signature[@ignore]);
}

and env_values_t = ((string, value) Hashtbl.t[@compare.ignore])

and environment = {
    values : (env_values_t [@ignore]);
  enclosing : (environment option [@ignore])
}

and value =
  | Bool of bool
  | Number of float
  | Nil
  | String of string
  | Callable of callable
[@@deriving compare, sexp_of]

let empty () : env_values_t = Hashtbl.create (module String)

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
                decl_environment = { values = empty (); enclosing = None };
                fn = (fun _ _ -> Number (Unix.gettimeofday ()));
              } );
        ];
    enclosing = None;
  }

type expr =
  | Binary of expr * token_kind * expr
  | Grouping of expr
  | Literal of value
  | Unary of token_kind * expr
  | Assign of token_kind * expr
  | Variable of token_kind
  | LogicalOr of expr * expr
  | LogicalAnd of expr * expr
  | Call of expr * token * expr list
[@@deriving compare, sexp_of]

type statement =
  | Expr of expr
  | Print of expr
  | Var of token_kind * expr
  | Block of statement array
  | Function of token * token list * statement list
  | Return of token * expr
  | IfStmt of expr * statement
  | IfElseStmt of expr * statement * statement
  | WhileStmt of expr * statement
[@@deriving sexp_of]

let rec sync acc = function
  (* | For :: _ as r -> *)
  (*     (acc, r) *)
  | ({ kind = SemiColon; _ } as t) :: (_ as r) -> (t :: acc, r)
  | [] -> (acc, [])
  | x :: r -> (sync [@tailcall]) (x :: acc) r

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
  | { kind = False; _ } :: rest -> Ok (Literal (Bool false), rest)
  | { kind = True; _ } :: rest -> Ok (Literal (Bool true), rest)
  | { kind = Nil; _ } :: rest -> Ok (Literal Nil, rest)
  | { kind = Number f; _ } :: rest -> Ok (Literal (Number f), rest)
  | { kind = String s; _ } :: rest -> Ok (Literal (String s), rest)
  | { kind = ParenLeft; _ } :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | { kind = ParenRight; _ } :: rest -> Ok (Grouping e, rest)
      | _ as rest -> error "Primary" "Expected closing parenthesis `)`" rest )
  | { kind = Identifier _ as i; _ } :: rest -> Ok (Variable i, rest)
  | _ as rest ->
      error "Primary" "Expected primary (e.g `1` or `(true)` or \"hello\")" rest

and fn_call tokens =
  let* expr, rest = primary tokens in
  match rest with
  | { kind = ParenLeft; _ } :: rest -> fn_call_arguments expr [] rest
  | _ -> Ok (expr, rest)

and fn_call_comma_argument = function
  | { kind = Comma; _ } :: rest -> expression rest
  | _ as rest ->
      error "Function call arguments" "Expected `,` before argument" rest

and fn_call_comma_arguments args = function
  | ({ kind = ParenRight; _ } as t) :: rest -> Ok (args, t, rest)
  | _ as rest ->
      let* arg, rest = fn_call_comma_argument rest in
      (fn_call_comma_arguments [@tailcall]) (arg :: args) rest

and fn_call_arguments callee args = function
  | ({ kind = ParenRight; _ } as t) :: rest -> Ok (Call (callee, t, args), rest)
  | _ as rest ->
      let* expr, rest = expression rest in
      let* args, closing_paren, rest = fn_call_comma_arguments [ expr ] rest in
      let len = List.length args in
      if len >= 255 then
        Stdlib.prerr_endline
          (Printf.sprintf
             "Function call: Too many arguments: limit is 255, got: %d" len);
      Ok (Call (callee, closing_paren, List.rev args), rest)

and unary = function
  | { kind = Bang as t; _ } :: rest | { kind = Minus as t; _ } :: rest ->
      let+ right, rest = unary rest in
      (Unary (t, right), rest)
  | _ as t -> (fn_call [@tailcall]) t

and multiplication tokens =
  let* left, rest = unary tokens in
  match rest with
  | { kind = Star as t; _ } :: rest | { kind = Slash as t; _ } :: rest ->
      let+ right, rest = multiplication rest in
      (Binary (left, t, right), rest)
  | _ -> Ok (left, rest)

and addition tokens =
  let* left, rest = multiplication tokens in
  match rest with
  | { kind = Plus as t; _ } :: rest | { kind = Minus as t; _ } :: rest ->
      let+ right, rest = addition rest in
      (Binary (left, t, right), rest)
  | _ -> Ok (left, rest)

and comparison tokens =
  let* left, rest = addition tokens in
  match rest with
  | { kind = Greater as t; _ } :: rest
  | { kind = GreaterEqual as t; _ } :: rest
  | { kind = Less as t; _ } :: rest
  | { kind = LessEqual as t; _ } :: rest ->
      let+ right, rest = comparison rest in
      (Binary (left, t, right), rest)
  | _ -> Ok (left, rest)

and equality tokens =
  let* left, rest = comparison tokens in
  match rest with
  | { kind = BangEqual as t; _ } :: rest | { kind = EqualEqual as t; _ } :: rest
    ->
      let+ right, rest = equality rest in
      (Binary (left, t, right), rest)
  | _ -> Ok (left, rest)

and expression tokens = (assignment [@tailcall]) tokens

and assignment = function
  | [] as rest -> error "Assignement" "Expected assignement (e.g `a = 1;`)" rest
  | _ as t -> (
      let* e, rest = logic_or t in
      match rest with
      | { kind = Equal; _ } :: rest -> (
          let* a, rest = assignment rest in
          match e with
          | Variable v -> Ok (Assign (v, a), rest)
          | _ -> error "Assignement" "Expected valid assignment target" t )
      | _ -> Ok (e, rest) )

and logic_and tokens =
  let* l, rest = equality tokens in
  match rest with
  | { kind = And; _ } :: rest ->
      let+ r, rest = logic_and rest in
      (LogicalAnd (l, r), rest)
  | [] ->
      error "Logical and expression" "Expected and (e.g `true and false`)" rest
  | _ -> Ok (l, rest)

and logic_or tokens =
  let* l, rest = logic_and tokens in
  match rest with
  | { kind = Or; _ } :: rest ->
      let+ r, rest = logic_or rest in
      (LogicalOr (l, r), rest)
  | [] -> error "Logical or expression" "Expected or (e.g `true or false`)" rest
  | _ -> Ok (l, rest)

and expression_stmt = function
  | [] as rest ->
      error "Expression statement" "Expected expression statement (e.g `1;`)"
        rest
  | _ as t -> (
      let* stmt, rest = expression t in
      match rest with
      | { kind = SemiColon; _ } :: rest -> Ok (stmt, rest)
      | _ as rest ->
          error "Expression statement" "Expected closing semicolon `;`" rest )

and print_stmt = function
  | { kind = Print; _ } :: rest ->
      let+ expr, rest = expression_stmt rest in
      (Print expr, rest)
  | _ as rest ->
      error "Print statement" "Expected print statement (e.g `print 1;`)" rest

and if_stmt = function
  | { kind = If; _ } :: { kind = ParenLeft; _ } :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | { kind = ParenRight; _ } :: rest -> (
          let* then_stmt, rest = statement rest in
          match rest with
          | { kind = Else; _ } :: rest ->
              let+ else_stmt, rest = statement rest in
              (IfElseStmt (e, then_stmt, else_stmt), rest)
          | _ -> Ok (IfStmt (e, then_stmt), rest) )
      | _ -> error "If statement" "Expected closing `)`" rest )
  | _ as rest ->
      error "If statement" "Expected if statement (e.g `if (true) print 3;`)"
        rest

and while_stmt = function
  | { kind = While; _ } :: { kind = ParenLeft; _ } :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | { kind = ParenRight; _ } :: rest ->
          let+ s, rest = statement rest in
          (WhileStmt (e, s), rest)
      | _ -> error "While statement" "Missing closing `)`" rest )
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
      let* init_clause, rest =
        match rest with
        | { kind = Var; _ } :: _ -> var_decl rest
        | { kind = SemiColon; _ } :: rest ->
            Ok (Expr (Literal (Bool true)), rest)
        | _ ->
            let+ e, rest = expression_stmt rest in
            (Expr e, rest)
      in
      let* stop_cond, rest =
        match rest with
        | { kind = SemiColon; _ } :: rest -> Ok (Literal (Bool true), rest)
        | _ -> (
            let* e, rest = expression rest in
            match rest with
            | { kind = SemiColon; _ } :: rest -> Ok (e, rest)
            | _ ->
                error "For-loop"
                  "Expected terminating semicolon after stop condition" rest )
      in
      let* incr_stmt, rest =
        match rest with
        | { kind = ParenRight; _ } :: _ -> Ok (Literal (Bool true), rest)
        | _ -> expression rest
      in
      let* _, rest =
        match rest with
        | { kind = ParenRight; _ } :: rest -> Ok (Nil, rest)
        | _ ->
            error "For-loop"
              "Expected closing parenthesis `)` after increment expression" rest
      in
      let* body, rest = statement rest in
      let* enclosed_body =
        Ok
          (Block
             [|
               init_clause;
               WhileStmt (stop_cond, Block [| body; Expr incr_stmt |]);
             |])
      in
      Ok (enclosed_body, rest)
  | _ as rest -> error "For-loop" "Expected loop (e.g `for (;;)`)" rest

and block_stmt_inner tokens acc =
  match tokens with
  | [] -> error "Block statement" "Expected closing `}`" tokens
  | { kind = CurlyBraceRight; _ } :: rest -> Ok (acc, rest)
  | _ ->
      let* s, rest = declaration tokens in
      let* acc = Ok (Array.append acc [| s |]) in
      block_stmt_inner rest acc

and block_stmt = function
  | { kind = CurlyBraceLeft; _ } :: rest ->
      let+ stmts, rest = block_stmt_inner rest [||] in
      (Block stmts, rest)
  | _ as rest ->
      error "Block statement" "Expected block statement with opening `{`" rest

and return_stmt = function
  | ({ kind = Return; _ } as ret) :: { kind = SemiColon; _ } :: rest ->
      Ok (Return (ret, Literal Nil), rest)
  | ({ kind = Return; _ } as ret) :: rest ->
      let* e, rest = expression rest in
      let* rest =
        match rest with
        | { kind = SemiColon; _ } :: rest -> Ok rest
        | _ as rest ->
            error "Return statement" "Expected terminating semicolon `;`" rest
      in
      Ok (Return (ret, e), rest)
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
      let+ e, rest = expression_stmt t in
      (Expr e, rest)

and var_decl = function
  | { kind = Var; _ }
    :: { kind = Identifier n; _ } :: { kind = Equal; _ } :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | { kind = SemiColon; _ } :: rest -> Ok (Var (Identifier n, e), rest)
      | _ -> error "Variable declaration" "Expected terminating `;`" rest )
  | { kind = Var; _ }
    :: { kind = Identifier n; _ } :: { kind = SemiColon; _ } :: rest ->
      Ok (Var (Identifier n, Literal Nil), rest)
  | _ as rest ->
      error "Variable declaration"
        "Expected variable declaration (e.g `var x = 1;`)" rest

and function_decl = function
  | { kind = Fun; _ }
    :: ({ kind = Identifier _; _ } as name) :: { kind = ParenLeft; _ } :: rest
    -> (
      let* args, rest = fn_decl_arguments [] rest in
      let* block, rest = block_stmt rest in
      match block with
      | Block statements ->
          Ok (Function (name, args, Array.to_list statements), rest)
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
      let* arg, rest = fn_decl_comma_argument rest in
      (fn_decl_comma_arguments [@tailcall]) (arg :: args) rest

and fn_decl_arguments args = function
  | { kind = ParenRight; _ } :: rest -> Ok (List.rev args, rest)
  | ({ kind = Identifier _; _ } as identifier) :: rest ->
      let* args, rest = fn_decl_comma_arguments [ identifier ] rest in
      let len = List.length args in
      if len >= 255 then
        Stdlib.prerr_endline
          (Printf.sprintf
             "Function definition: Too many arguments: limit is 255, got: %d"
             len);
      Ok (List.rev args, rest)
  | _ as rest ->
      error "Function definition" "Expected argument list (e.g `(a, b)`)" rest

and declaration d =
  match d with
  | { kind = Var; _ } :: _ -> (var_decl [@tailcall]) d
  | { kind = Fun; _ } :: _ -> (function_decl [@tailcall]) d
  | _ -> (statement [@tailcall]) d

and program decls = function
  | [] -> decls
  | _ as t ->
      let d = declaration t in
      let rest =
        match d with Ok (_, rest) -> rest | Error (_, rest) -> rest
      in
      let ok_or_err = Result.map ~f:fst d |> Result.map_error ~f:fst in
      let decls = ok_or_err :: decls in
      (program [@tailcall]) decls rest

let parse tokens = program [] tokens |> List.rev |> Result.combine_errors

let value_to_string = function
  | String s -> s
  | Number f -> Float.to_string f
  | Bool true -> "true"
  | Bool false -> "false"
  | Nil -> "nil"
  | Callable { name = n; _ } -> "function@" ^ n
