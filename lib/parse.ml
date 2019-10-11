open Sexplib.Std

type value = Bool of bool | Number of float | Nil | String of string
[@@deriving sexp]

type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of value
  | Unary of Lex.lex_token * expr
  | Variable of Lex.lex_token
  | LogicalOr of expr * expr
  | LogicalAnd of expr * expr
[@@deriving sexp]

type statement =
  | Expr of expr
  | Print of expr
  | Var of Lex.lex_token * expr
  | Block of statement array
  | IfStmt of expr * statement
  | IfElseStmt of expr * statement * statement
  | WhileStmt of expr * statement

let rec primary = function
  | [] ->
      failwith "No more tokens to match for a primary"
  | Lex.False :: rest ->
      (Literal (Bool false), rest)
  | Lex.True :: rest ->
      (Literal (Bool true), rest)
  | Lex.Nil :: rest ->
      (Literal Nil, rest)
  | Lex.Number f :: rest ->
      (Literal (Number f), rest)
  | Lex.String s :: rest ->
      (Literal (String s), rest)
  | Lex.ParenLeft :: rest -> (
      let e, rrest = expression rest in
      match rrest with
      | Lex.ParenRight :: rrrest ->
          (Grouping e, rrrest)
      | _ ->
          failwith "Missing closing parenthesis" )
  | (Lex.Identifier _ as i) :: rest ->
      (Variable i, rest)
  | x :: _ ->
      failwith
        ("Not a primary: " ^ Base.Sexp.to_string_hum (Lex.sexp_of_lex_token x))

and unary = function
  | (Lex.Bang as t) :: rest | (Lex.Minus as t) :: rest ->
      let right, rrest = unary rest in
      (Unary (t, right), rrest)
  | _ as t ->
      primary t

and multiplication tokens =
  let left, rest = unary tokens in
  match rest with
  | (Lex.Star as t) :: rrest | (Lex.Slash as t) :: rrest ->
      let right, rrrest = multiplication rrest in
      (Binary (left, t, right), rrrest)
  | _ ->
      (left, rest)

and addition tokens =
  let left, rest = multiplication tokens in
  match rest with
  | (Lex.Plus as t) :: rrest | (Lex.Minus as t) :: rrest ->
      let right, rrrest = addition rrest in
      (Binary (left, t, right), rrrest)
  | _ ->
      (left, rest)

and comparison tokens =
  let left, rest = addition tokens in
  match rest with
  | (Lex.Greater as t) :: rrest
  | (Lex.GreaterEqual as t) :: rrest
  | (Lex.Less as t) :: rrest
  | (Lex.LessEqual as t) :: rrest ->
      let right, rrrest = comparison rrest in
      (Binary (left, t, right), rrrest)
  | _ ->
      (left, rest)

and equality tokens =
  let left, rest = comparison tokens in
  match rest with
  | (Lex.BangEqual as t) :: rrest | (Lex.EqualEqual as t) :: rrest ->
      let right, rrrest = equality rrest in
      (Binary (left, t, right), rrrest)
  | _ ->
      (left, rest)

and expression tokens = assignment tokens

and assignment tokens = logic_or tokens

and logic_and tokens =
  let l, rest = equality tokens in
  match rest with
  | Lex.And :: rest ->
      let r, rest = logic_and rest in
      (LogicalAnd (l, r), rest)
  | [] ->
      failwith "No more tokens to match for a logic_or expression"
  | _ ->
      (l, rest)

and logic_or tokens =
  let l, rest = logic_and tokens in
  match rest with
  | Lex.Or :: rest ->
      let r, rest = logic_or rest in
      (LogicalOr (l, r), rest)
  | [] ->
      failwith "No more tokens to match for a logic_or expression"
  | _ ->
      (l, rest)

and expression_stmt = function
  | [] ->
      failwith "No more tokens to match for an expression statement"
  | _ as t -> (
      let stmt, rest = expression t in
      match rest with
      | Lex.SemiColon :: rrest ->
          (stmt, rrest)
      | x :: _ ->
          failwith
            ( "Missing semicolon after statement: expected `;`, got: "
            ^ Base.Sexp.to_string_hum (Lex.sexp_of_lex_token x) )
      | _ ->
          failwith "Missing semicolon after statement: no more tokens " )

and print_stmt = function
  | [] ->
      failwith "No more tokens to match for a print statement"
  | Lex.Print :: rest ->
      let expr, rrest = expression_stmt rest in
      (Print expr, rrest)
  | x :: _ ->
      failwith
        ( "Missing print to match a print statement: "
        ^ Base.Sexp.to_string_hum (Lex.sexp_of_lex_token x) )

and if_stmt = function
  | [] ->
      failwith "No more tokens to match for a if statement"
  | Lex.If :: Lex.ParenLeft :: rest -> (
      let e, rest = expression rest in
      match rest with
      | Lex.ParenRight :: rest -> (
          let then_stmt, rest = statement rest in
          match rest with
          | Lex.Else :: rest ->
              let else_stmt, rest = statement rest in
              (IfElseStmt (e, then_stmt, else_stmt), rest)
          | _ ->
              (IfStmt (e, then_stmt), rest) )
      | _ ->
          failwith "Missing closing parenthesis in if statement" )
  | _ ->
      failwith "Wrong call to if_stmt: not an if statement"

and while_stmt = function
  | [] ->
      failwith "No more tokens to match for a while statement"
  | Lex.While :: Lex.ParenLeft :: rest -> (
      let e, rest = expression rest in
      match rest with
      | Lex.ParenRight :: rest ->
          let s, rest = statement rest in
          (WhileStmt (e, s), rest)
      | _ ->
          failwith "Missing closing parenthesis in if statement" )
  | _ ->
      failwith "Wrong call to while_stmt: not an while statement"

and for_stmt = function
  | [] ->
      failwith "No more tokens to match for a for-loop statement"
  | Lex.For
    :: Lex.ParenLeft
       :: Lex.SemiColon :: Lex.SemiColon :: Lex.ParenRight :: rest ->
      let s, rest = statement rest in
      (WhileStmt (Literal (Bool true), s), rest)
  | Lex.For :: Lex.ParenLeft :: (Lex.Var :: _ as var) -> (
      let v, rest = var_decl var in
      let stop_cond, rest = expression rest in
      match rest with
      | Lex.SemiColon :: rest -> (
          let increment, rest = expression rest in
          match rest with
          | Lex.ParenRight :: rest ->
              let body, rest = statement rest in
              let enclosed_body =
                Block [|v; WhileStmt (stop_cond, body); Expr increment|]
              in
              (enclosed_body, rest)
          | x :: _ ->
              failwith
                ( "Missing closing parenthesis in if statement, got: "
                ^ Base.Sexp.to_string_hum (Lex.sexp_of_lex_token x) )
          | [] ->
              failwith
                "Missing closing parenthesis in if statement, no more tokens" )
      | _ ->
          failwith "Missing semicolon after condition in for-loop declaration"
      )
  | _ ->
      failwith "Wrong call to loop_stmt: not an loop statement"

and block_stmt_inner tokens acc =
  match tokens with
  | [] ->
      failwith "No more tokens to match for a block statement"
  | Lex.CurlyBraceRight :: rest ->
      (acc, rest)
  | _ ->
      let s, rest = declaration tokens in
      let acc = Array.append acc [|s|] in
      block_stmt_inner rest acc

and block_stmt = function
  | [] ->
      failwith "No more tokens to match for a block statement"
  | Lex.CurlyBraceLeft :: rest ->
      let stmts, rest = block_stmt_inner rest [||] in
      (Block stmts, rest)
  | _ ->
      failwith "Wrong call to block_stmt: not a block statement"

and statement = function
  | [] ->
      failwith "No more tokens to match for a statement"
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
      let e, rest = expression_stmt t in
      (Expr e, rest)

and var_decl = function
  | [] ->
      failwith "No more tokens to match for a variable declaration"
  | Lex.Var :: Lex.Identifier n :: Lex.Equal :: rest -> (
      let e, rrest = expression rest in
      match rrest with
      | Lex.SemiColon :: rrrest ->
          (Var (Lex.Identifier n, e), rrrest)
      | x :: _ ->
          failwith
            ( "Missing semicolon after variable declaration, got: "
            ^ Base.Sexp.to_string_hum (Lex.sexp_of_lex_token x) )
      | [] ->
          failwith
            "Missing semicolon after variable declaration, no more tokens" )
  | Lex.Var :: Lex.Identifier n :: Lex.SemiColon :: rest ->
      (Var (Lex.Identifier n, Literal Nil), rest)
  | x :: _ ->
      failwith
        ( "Malformed variable declaration: "
        ^ Base.Sexp.to_string_hum (Lex.sexp_of_lex_token x) )

and declaration d =
  match d with Lex.Var :: _ -> var_decl d | _ -> statement d

and program decls = function
  | [] ->
      decls
  | _ as t ->
      let decl, rest = declaration t in
      let decls = Base.Array.append decls [|decl|] in
      program decls rest

let parse tokens =
  let stmts = [||] in
  program stmts tokens
