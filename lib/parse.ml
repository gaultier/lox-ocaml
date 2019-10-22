open Base.Result

let ( let* ) x f = Result.bind x f

let ( let+ ) x f = Result.map f x

type value = Bool of bool | Number of float | Nil | String of string

type expr =
  | Binary of expr * Lex.token_kind * expr
  | Grouping of expr
  | Literal of value
  | Unary of Lex.token_kind * expr
  | Assign of Lex.token_kind * expr
  | Variable of Lex.token_kind
  | LogicalOr of expr * expr
  | LogicalAnd of expr * expr

type statement =
  | Expr of expr
  | Print of expr
  | Var of Lex.token_kind * expr
  | Block of statement array
  | IfStmt of expr * statement
  | IfElseStmt of expr * statement * statement
  | WhileStmt of expr * statement

let rec sync acc = function
  (* | Lex.For :: _ as r -> *)
  (*     (acc, r) *)
  | ({Lex.kind= Lex.SemiColon; _} as t) :: (_ as r) ->
      (t :: acc, r)
  | [] ->
      (acc, [])
  | x :: r ->
      (sync [@tailcall]) (x :: acc) r

let error ctx expected rest =
  let head =
    Base.List.hd rest
    |> Base.Option.value
         ~default:{Lex.lines= 1; Lex.columns= 1; Lex.kind= Lex.Class}
  in
  let invalid, rest = sync [] rest in
  let invalid_s =
    match invalid with
    | [] ->
        "no more tokens"
    | _ :: _ ->
        invalid
        |> Base.List.rev_map ~f:(fun {Lex.kind= x; _} -> Lex.token_to_string x)
        |> Base.List.fold ~init:"" ~f:(fun acc x -> acc ^ " " ^ x)
        |> String.trim
  in
  fail
    ( Printf.sprintf "%d:%d:Context: %s. %s. Got: `%s`." head.lines
        head.columns ctx expected invalid_s
    , rest )

let rec primary = function
  | {Lex.kind= Lex.False; _} :: rest ->
      Ok (Literal (Bool false), rest)
  | {Lex.kind= Lex.True; _} :: rest ->
      Ok (Literal (Bool true), rest)
  | {Lex.kind= Lex.Nil; _} :: rest ->
      Ok (Literal Nil, rest)
  | {Lex.kind= Lex.Number f; _} :: rest ->
      Ok (Literal (Number f), rest)
  | {Lex.kind= Lex.String s; _} :: rest ->
      Ok (Literal (String s), rest)
  | {Lex.kind= Lex.ParenLeft; _} :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | {Lex.kind= Lex.ParenRight; _} :: rest ->
          Ok (Grouping e, rest)
      | _ as rest ->
          error "Primary" "Expected closing parenthesis `)`" rest )
  | {Lex.kind= Lex.Identifier _ as i; _} :: rest ->
      Ok (Variable i, rest)
  | _ as rest ->
      error "Primary" "Expected primary (e.g `1` or `(true)` or \"hello\")"
        rest

and unary = function
  | {Lex.kind= Lex.Bang as t; _} :: rest
  | {Lex.kind= Lex.Minus as t; _} :: rest ->
      let+ right, rest = unary rest in
      (Unary (t, right), rest)
  | _ as t ->
      (primary [@tailcall]) t

and multiplication tokens =
  let* left, rest = unary tokens in
  match rest with
  | {Lex.kind= Lex.Star as t; _} :: rest
  | {Lex.kind= Lex.Slash as t; _} :: rest ->
      let+ right, rest = multiplication rest in
      (Binary (left, t, right), rest)
  | _ ->
      Ok (left, rest)

and addition tokens =
  let* left, rest = multiplication tokens in
  match rest with
  | {Lex.kind= Lex.Plus as t; _} :: rest
  | {Lex.kind= Lex.Minus as t; _} :: rest ->
      let+ right, rest = addition rest in
      (Binary (left, t, right), rest)
  | _ ->
      Ok (left, rest)

and comparison tokens =
  let* left, rest = addition tokens in
  match rest with
  | {Lex.kind= Lex.Greater as t; _} :: rest
  | {Lex.kind= Lex.GreaterEqual as t; _} :: rest
  | {Lex.kind= Lex.Less as t; _} :: rest
  | {Lex.kind= Lex.LessEqual as t; _} :: rest ->
      let+ right, rest = comparison rest in
      (Binary (left, t, right), rest)
  | _ ->
      Ok (left, rest)

and equality tokens =
  let* left, rest = comparison tokens in
  match rest with
  | {Lex.kind= Lex.BangEqual as t; _} :: rest
  | {Lex.kind= Lex.EqualEqual as t; _} :: rest ->
      let+ right, rest = equality rest in
      (Binary (left, t, right), rest)
  | _ ->
      Ok (left, rest)

and expression tokens = (assignment [@tailcall]) tokens

and assignment = function
  | [] as rest ->
      error "Assignement" "Expected assignement (e.g `a = 1;`)" rest
  | _ as t -> (
      let* e, rest = logic_or t in
      match rest with
      | {Lex.kind= Lex.Equal; _} :: rest -> (
          let* a, rest = assignment rest in
          match e with
          | Variable v ->
              Ok (Assign (v, a), rest)
          | _ ->
              error "Assignement" "Expected valid assignment target" rest )
      | _ ->
          Ok (e, rest) )

and logic_and tokens =
  let* l, rest = equality tokens in
  match rest with
  | {Lex.kind= Lex.And; _} :: rest ->
      let+ r, rest = logic_and rest in
      (LogicalAnd (l, r), rest)
  | [] ->
      error "Logical and expression" "Expected and (e.g `true and false`)" rest
  | _ ->
      Ok (l, rest)

and logic_or tokens =
  let* l, rest = logic_and tokens in
  match rest with
  | {Lex.kind= Lex.Or; _} :: rest ->
      let+ r, rest = logic_or rest in
      (LogicalOr (l, r), rest)
  | [] ->
      error "Logical or expression" "Expected or (e.g `true or false`)" rest
  | _ ->
      Ok (l, rest)

and expression_stmt = function
  | [] as rest ->
      error "Expression statement" "Expected expression statement (e.g `1;`)"
        rest
  | _ as t -> (
      let* stmt, rest = expression t in
      match rest with
      | {Lex.kind= Lex.SemiColon; _} :: rest ->
          Ok (stmt, rest)
      | _ as rest ->
          error "Expression statement" "Expected closing semicolon `;`" rest )

and print_stmt = function
  | {Lex.kind= Lex.Print; _} :: rest ->
      let+ expr, rest = expression_stmt rest in
      (Print expr, rest)
  | _ as rest ->
      error "Print statement" "Expected print statement (e.g `print 1;`)" rest

and if_stmt = function
  | {Lex.kind= Lex.If; _} :: {Lex.kind= Lex.ParenLeft; _} :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | {Lex.kind= Lex.ParenRight; _} :: rest -> (
          let* then_stmt, rest = statement rest in
          match rest with
          | {Lex.kind= Lex.Else; _} :: rest ->
              let+ else_stmt, rest = statement rest in
              (IfElseStmt (e, then_stmt, else_stmt), rest)
          | _ ->
              Ok (IfStmt (e, then_stmt), rest) )
      | _ ->
          error "If statement" "Expected closing `)`" rest )
  | _ as rest ->
      error "If statement" "Expected if statement (e.g `if (true) print 3;`)"
        rest

and while_stmt = function
  | {Lex.kind= Lex.While; _} :: {Lex.kind= Lex.ParenLeft; _} :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | {Lex.kind= Lex.ParenRight; _} :: rest ->
          let+ s, rest = statement rest in
          (WhileStmt (e, s), rest)
      | _ ->
          error "While statement" "Missing closing `)`" rest )
  | _ as rest ->
      error "While statement"
        "Expected while statement: (e.g `while(true) {}`)" rest

and for_stmt = function
  (* for (;;) *)
  | {Lex.kind= Lex.For; _}
    :: {Lex.kind= Lex.ParenLeft; _}
       :: {Lex.kind= Lex.SemiColon; _}
          :: {Lex.kind= Lex.SemiColon; _}
             :: {Lex.kind= Lex.ParenRight; _} :: rest ->
      let+ s, rest = statement rest in
      (WhileStmt (Literal (Bool true), s), rest)
  (* TODO: partial for-loop declaration e.g *)
  (* for (;i; i = i+1) *)
  (* for (;i;) *)
  (* or for (i;;i=i+1)  *)
  (* or for (;;i=i+1)  *)

  (* for (var i = 0; i < 5; i = i + 1) *)
  | {Lex.kind= Lex.For; _}
    :: {Lex.kind= Lex.ParenLeft; _} :: ({Lex.kind= Lex.Var; _} :: _ as var)
    -> (
      let* v, rest = var_decl var in
      (* FIXME *)
      let* stop_cond, rest = expression rest in
      match rest with
      | {Lex.kind= Lex.SemiColon; _} :: rest -> (
          let* increment, rest = expression rest in
          match rest with
          | {Lex.kind= Lex.ParenRight; _} :: rest ->
              let* body, rest = statement rest in
              let* enclosed_body =
                Ok
                  (Block
                     [| v
                      ; WhileStmt (stop_cond, Block [|body; Expr increment|])
                     |])
              in
              Ok (enclosed_body, rest)
          | _ as rest ->
              error "For-loop"
                "Expected closing parenthesis `)` after increment expression"
                rest )
      | _ as rest ->
          error "For-loop" "Expected semicolon `;` after stop condition" rest )
  | _ as rest ->
      error "For-loop" "Expected loop (e.g `for (;;)`)" rest

and block_stmt_inner tokens acc =
  match tokens with
  | [] ->
      error "Block statement" "Expected closing `}`" tokens
  | {Lex.kind= Lex.CurlyBraceRight; _} :: rest ->
      Ok (acc, rest)
  | _ ->
      let* s, rest = declaration tokens in
      let* acc = Ok (Array.append acc [|s|]) in
      block_stmt_inner rest acc

and block_stmt = function
  | {Lex.kind= Lex.CurlyBraceLeft; _} :: rest ->
      let+ stmts, rest = block_stmt_inner rest [||] in
      (Block stmts, rest)
  | _ as rest ->
      error "Block statement" "Expected block statement with opening `{`" rest

and statement = function
  | [] as rest ->
      error "Statement" "Expected statement (e.g `x = 1;`)" rest
  | {Lex.kind= Lex.Print; _} :: _ as t ->
      print_stmt t
  | {Lex.kind= Lex.CurlyBraceLeft; _} :: _ as t ->
      block_stmt t
  | {Lex.kind= Lex.If; _} :: _ as t ->
      if_stmt t
  | {Lex.kind= Lex.While; _} :: _ as t ->
      while_stmt t
  | {Lex.kind= Lex.For; _} :: _ as t ->
      for_stmt t
  | _ as t ->
      let+ e, rest = expression_stmt t in
      (Expr e, rest)

and var_decl = function
  | {Lex.kind= Lex.Var; _}
    :: {Lex.kind= Lex.Identifier n; _} :: {Lex.kind= Lex.Equal; _} :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | {Lex.kind= Lex.SemiColon; _} :: rest ->
          Ok (Var (Lex.Identifier n, e), rest)
      | _ ->
          error "Variable declaration" "Expected terminating `;`" rest )
  | {Lex.kind= Lex.Var; _}
    :: {Lex.kind= Lex.Identifier n; _} :: {Lex.kind= Lex.SemiColon; _} :: rest
    ->
      Ok (Var (Lex.Identifier n, Literal Nil), rest)
  | _ as rest ->
      error "Variable declaration"
        "Expected variable declaration (e.g `var x = 1;`)" rest

and declaration d =
  match d with
  | {Lex.kind= Lex.Var; _} :: _ ->
      (var_decl [@tailcall]) d
  | _ ->
      (statement [@tailcall]) d

and program decls = function
  | [] ->
      decls
  | _ as t ->
      let d = declaration t in
      let rest =
        match d with Ok (_, rest) -> rest | Error (_, rest) -> rest
      in
      let ok_or_err = map ~f:fst d |> map_error ~f:fst in
      let decls = Base.Array.append decls [|ok_or_err|] in
      (program [@tailcall]) decls rest

let parse tokens = program [||] tokens |> Base.Array.to_list |> combine_errors

let value_to_string = function
  | String s ->
      s (* No quotes for human readability *)
  | Number f ->
      Float.to_string f
  | Bool true ->
      "true"
  | Bool false ->
      "false"
  | Nil ->
      "nil"
