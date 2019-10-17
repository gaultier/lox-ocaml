open Base.Result

let ( let* ) x f = Result.bind x f

let ( let+ ) x f = Result.map f x

type value = Bool of bool | Number of float | Nil | String of string

type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of value
  | Unary of Lex.lex_token * expr
  | Assign of Lex.lex_token * expr
  | Variable of Lex.lex_token
  | LogicalOr of expr * expr
  | LogicalAnd of expr * expr

type statement =
  | Expr of expr
  | Print of expr
  | Var of Lex.lex_token * expr
  | Block of statement array
  | IfStmt of expr * statement
  | IfElseStmt of expr * statement * statement
  | WhileStmt of expr * statement

let rec sync acc = function
  (* | Lex.For :: _ as r -> *)
  (*     (acc, r) *)
  | (Lex.SemiColon as t) :: (_ as r) ->
      (t :: acc, r)
  | [] ->
      (acc, [])
  | x :: r ->
      sync (x :: acc) r

let error ctx expected rest =
  let invalid, rest = sync [] rest in
  let invalid_s =
    match invalid with
    | [] ->
        "no more tokens"
    | _ ->
        invalid
        |> Base.List.rev_map ~f:(fun _ -> "*")
        |> Base.List.fold ~init:"" ~f:(fun acc x -> acc ^ " " ^ x)
  in
  fail (Printf.sprintf "Context: %s. %s. Got:%s." ctx expected invalid_s, rest)

let rec primary = function
  | Lex.False :: rest ->
      Ok (Literal (Bool false), rest)
  | Lex.True :: rest ->
      Ok (Literal (Bool true), rest)
  | Lex.Nil :: rest ->
      Ok (Literal Nil, rest)
  | Lex.Number f :: rest ->
      Ok (Literal (Number f), rest)
  | Lex.String s :: rest ->
      Ok (Literal (String s), rest)
  | Lex.ParenLeft :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | Lex.ParenRight :: rest ->
          Ok (Grouping e, rest)
      | _ as rest ->
          error "Primary" "Missing closing parenthesis `)`" rest )
  | (Lex.Identifier _ as i) :: rest ->
      Ok (Variable i, rest)
  | _ as rest ->
      error "Primary" "Malformed primary (e.g `1` or `(true)` or \"hello\")"
        rest

and unary = function
  | (Lex.Bang as t) :: rest | (Lex.Minus as t) :: rest ->
      let+ right, rest = unary rest in
      (Unary (t, right), rest)
  | _ as t ->
      primary t

and multiplication tokens =
  let* left, rest = unary tokens in
  match rest with
  | (Lex.Star as t) :: rest | (Lex.Slash as t) :: rest ->
      let+ right, rest = multiplication rest in
      (Binary (left, t, right), rest)
  | _ ->
      Ok (left, rest)

and addition tokens =
  let* left, rest = multiplication tokens in
  match rest with
  | (Lex.Plus as t) :: rest | (Lex.Minus as t) :: rest ->
      let+ right, rest = addition rest in
      (Binary (left, t, right), rest)
  | _ ->
      Ok (left, rest)

and comparison tokens =
  let* left, rest = addition tokens in
  match rest with
  | (Lex.Greater as t) :: rest
  | (Lex.GreaterEqual as t) :: rest
  | (Lex.Less as t) :: rest
  | (Lex.LessEqual as t) :: rest ->
      let+ right, rest = comparison rest in
      (Binary (left, t, right), rest)
  | _ ->
      Ok (left, rest)

and equality tokens =
  let* left, rest = comparison tokens in
  match rest with
  | (Lex.BangEqual as t) :: rest | (Lex.EqualEqual as t) :: rest ->
      let+ right, rest = equality rest in
      (Binary (left, t, right), rest)
  | _ ->
      Ok (left, rest)

and expression tokens = assignment tokens

and assignment = function
  | [] as rest ->
      error "Assignement" "Malformed assignement (e.g `a = 1;`)" rest
  | _ as t -> (
      let* e, rest = logic_or t in
      match rest with
      | Lex.Equal :: rest -> (
          let* a, rest = assignment rest in
          match e with
          | Variable v ->
              Ok (Assign (v, a), rest)
          | _ ->
              error "Assignement" "Invalid assignment target" rest )
      | _ ->
          Ok (e, rest) )

and logic_and tokens =
  let* l, rest = equality tokens in
  match rest with
  | Lex.And :: rest ->
      let+ r, rest = logic_and rest in
      (LogicalAnd (l, r), rest)
  | [] ->
      error "Logical and expression" "Malformed and (e.g `true and false`)"
        rest
  | _ ->
      Ok (l, rest)

and logic_or tokens =
  let* l, rest = logic_and tokens in
  match rest with
  | Lex.Or :: rest ->
      let+ r, rest = logic_or rest in
      (LogicalOr (l, r), rest)
  | [] ->
      error "Logical or expression" "Malformed or (e.g `true or false`)" rest
  | _ ->
      Ok (l, rest)

and expression_stmt = function
  | [] as rest ->
      error "Expression statement" "Malformed expression statement (e.g `1;`)"
        rest
  | _ as t -> (
      let* stmt, rest = expression t in
      match rest with
      | Lex.SemiColon :: rest ->
          Ok (stmt, rest)
      | _ as rest ->
          error "Expression statement" "missing closing semicolon" rest )

and print_stmt = function
  | Lex.Print :: rest ->
      let+ expr, rest = expression_stmt rest in
      (Print expr, rest)
  | _ as rest ->
      error "Print statement" "print statement" rest

and if_stmt = function
  | Lex.If :: Lex.ParenLeft :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | Lex.ParenRight :: rest -> (
          let* then_stmt, rest = statement rest in
          match rest with
          | Lex.Else :: rest ->
              let+ else_stmt, rest = statement rest in
              (IfElseStmt (e, then_stmt, else_stmt), rest)
          | _ ->
              Ok (IfStmt (e, then_stmt), rest) )
      | _ ->
          error "If statement" "Missing closing `)`" rest )
  | _ as rest ->
      error "If statement" "Malformed if statement (e.g `if (true) print 3;`)"
        rest

and while_stmt = function
  | Lex.While :: Lex.ParenLeft :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | Lex.ParenRight :: rest ->
          let+ s, rest = statement rest in
          (WhileStmt (e, s), rest)
      | _ ->
          error "While statement" "Missing closing `)` in the while condition"
            rest )
  | _ as rest ->
      error "While statement"
        "Malformed while statement: (e.g `while(true) {}`)" rest

and for_stmt = function
  (* for (;;) *)
  | Lex.For
    :: Lex.ParenLeft
       :: Lex.SemiColon :: Lex.SemiColon :: Lex.ParenRight :: rest ->
      let+ s, rest = statement rest in
      (WhileStmt (Literal (Bool true), s), rest)
  (* TODO: partial for-loop declaration e.g *)
  (* for (;i; i = i+1) *)
  (* for (;i;) *)
  (* or for (i;;i=i+1)  *)
  (* or for (;;i=i+1)  *)

  (* for (var i = 0; i < 5; i = i + 1) *)
  | Lex.For :: Lex.ParenLeft :: (Lex.Var :: _ as var) -> (
      let* v, rest = var_decl var in
      (* FIXME *)
      let* stop_cond, rest = expression rest in
      match rest with
      | Lex.SemiColon :: rest -> (
          let* increment, rest = expression rest in
          match rest with
          | Lex.ParenRight :: rest ->
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
              error "Loop"
                "Missing closing parenthesis `)` after increment expression"
                rest )
      | _ as rest ->
          error "Loop" "Missing semicolon `;` after stop condition" rest )
  | _ as rest ->
      error "Loop" "Malformed loop (e.g `for (;;)`)" rest

and block_stmt_inner tokens acc =
  match tokens with
  | [] ->
      error "Block statement" "expected closing `}`" tokens
  | Lex.CurlyBraceRight :: rest ->
      Ok (acc, rest)
  | _ ->
      let* s, rest = declaration tokens in
      let* acc = Ok (Array.append acc [|s|]) in
      block_stmt_inner rest acc

and block_stmt :
       Lex.lex_token list
    -> (statement * Lex.lex_token list, string * Lex.lex_token list) result =
  function
  | Lex.CurlyBraceLeft :: rest ->
      let+ stmts, rest = block_stmt_inner rest [||] in
      (Block stmts, rest)
  | _ as rest ->
      error "Block statement" "malformed block statement, expected opening `{`"
        rest

and statement :
       Lex.lex_token list
    -> (statement * Lex.lex_token list, string * Lex.lex_token list) result =
  function
  | [] as rest ->
      error "Statement" "statement (e.g `x = 1;`)" rest
  | Lex.Print :: _ as t ->
      print_stmt t
  | Lex.CurlyBraceLeft :: _ as t ->
      block_stmt t
  | Lex.If :: _ as t ->
      if_stmt t
  | Lex.While :: _ as t ->
      while_stmt t
  | Lex.For :: _ as t ->
      for_stmt t
  | _ as t ->
      let+ e, rest = expression_stmt t in
      (Expr e, rest)

and var_decl :
       Lex.lex_token list
    -> (statement * Lex.lex_token list, string * Lex.lex_token list) result =
  function
  | Lex.Var :: Lex.Identifier n :: Lex.Equal :: rest -> (
      let* e, rest = expression rest in
      match rest with
      | Lex.SemiColon :: rest ->
          Ok (Var (Lex.Identifier n, e), rest)
      | _ ->
          error "variable declaration" "semicolon" rest )
  | Lex.Var :: Lex.Identifier n :: Lex.SemiColon :: rest ->
      Ok (Var (Lex.Identifier n, Literal Nil), rest)
  | _ as rest ->
      error "Malformed variable declaration"
        "variable declaration (e.g `var x = 1;`)" rest

and declaration d :
    (statement * Lex.lex_token list, string * Lex.lex_token list) result =
  match d with Lex.Var :: _ -> var_decl d | _ -> statement d

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
      program decls rest

let parse tokens = program [||] tokens |> Base.Array.to_list |> combine_errors
