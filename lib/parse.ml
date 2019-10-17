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
  | Lex.For :: _ as r ->
      (acc, r)
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
  fail
    ( Printf.sprintf "Context: %s. Expected: %s. Got:%s." ctx expected invalid_s
    , rest )

let make_result_fixme x = Ok x

let extract_value_from_result_fixme x = Result.get_ok x

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
      let e, rest = expression rest in
      match rest with
      | Lex.ParenRight :: rest ->
          (Grouping e, rest)
      | _ :: _ ->
          failwith ("Missing closing parenthesis for primary, got: " ^ "*")
      | [] ->
          failwith "Missing closing parenthesis for primary, no more tokens" )
  | (Lex.Identifier _ as i) :: rest ->
      (Variable i, rest)
  | _ :: _ ->
      failwith ("Not a primary: " ^ "*")

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

and assignment = function
  | [] ->
      failwith "No more tokens to match for assignement"
  | _ as t -> (
      let e, rest = logic_or t in
      match rest with
      | Lex.Equal :: rest -> (
          let a, rest = assignment rest in
          match e with
          | Variable v ->
              (Assign (v, a), rest)
          | _ ->
              failwith "Invalid assignment target" )
      | _ ->
          (e, rest) )

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
      | _ :: _ ->
          failwith
            ( "Missing semicolon after expression statement: expected `;`, got: "
            ^ "*" )
      | _ ->
          failwith
            "Missing semicolon after expression statement: no more tokens " )

and print_stmt :
       Lex.lex_token list
    -> (statement * Lex.lex_token list, string * Lex.lex_token list) result =
  function
  | Lex.Print :: rest ->
      let expr, rest = expression_stmt rest in
      Ok (Print expr, rest)
  | _ as rest ->
      error "Print statement" "print statement" rest

and if_stmt : Lex.lex_token list -> statement * Lex.lex_token list = function
  | [] ->
      failwith "No more tokens to match for a if statement"
  | Lex.If :: Lex.ParenLeft :: rest -> (
      let e, rest = expression rest in
      match rest with
      | Lex.ParenRight :: rest -> (
          let then_stmt, rest =
            statement rest |> extract_value_from_result_fixme
          in
          match rest with
          | Lex.Else :: rest ->
              let else_stmt, rest =
                statement rest |> extract_value_from_result_fixme
              in
              (IfElseStmt (e, then_stmt, else_stmt), rest)
          | _ ->
              (IfStmt (e, then_stmt), rest) )
      | _ ->
          failwith "Missing closing parenthesis in if statement" )
  | _ ->
      failwith "Wrong call to if_stmt: not an if statement"

and while_stmt : Lex.lex_token list -> statement * Lex.lex_token list =
  function
  | [] ->
      failwith "No more tokens to match for a while statement"
  | Lex.While :: Lex.ParenLeft :: rest -> (
      let e, rest = expression rest in
      match rest with
      | Lex.ParenRight :: rest ->
          let s, rest = statement rest |> extract_value_from_result_fixme in
          (WhileStmt (e, s), rest)
      | _ ->
          failwith "Missing closing parenthesis in if statement" )
  | _ ->
      failwith "Wrong call to while_stmt: not an while statement"

and for_stmt : Lex.lex_token list -> statement * Lex.lex_token list = function
  | [] ->
      failwith "No more tokens to match for a for-loop statement"
  (* for (;;) *)
  | Lex.For
    :: Lex.ParenLeft
       :: Lex.SemiColon :: Lex.SemiColon :: Lex.ParenRight :: rest ->
      let s, rest = statement rest |> extract_value_from_result_fixme in
      (WhileStmt (Literal (Bool true), s), rest)
  (* TODO: partial for-loop declaration e.g *)
  (* for (;i; i = i+1) *)
  (* for (;i;) *)
  (* or for (i;;i=i+1)  *)
  (* or for (;;i=i+1)  *)

  (* for (var i = 0; i < 5; i = i + 1) *)
  | Lex.For :: Lex.ParenLeft :: (Lex.Var :: _ as var) -> (
      let v, rest = var_decl var |> Result.get_ok in
      (* FIXME *)
      let stop_cond, rest = expression rest in
      match rest with
      | Lex.SemiColon :: rest -> (
          let increment, rest = expression rest in
          match rest with
          | Lex.ParenRight :: rest ->
              let body, rest =
                statement rest |> extract_value_from_result_fixme
              in
              let enclosed_body =
                Block
                  [|v; WhileStmt (stop_cond, Block [|body; Expr increment|])|]
              in
              (enclosed_body, rest)
          | _ :: _ ->
              failwith
                ( "Missing closing parenthesis in for-loop declaration, got: "
                ^ "*" )
          | [] ->
              failwith
                "Missing closing parenthesis in for-loop declaration, no more \
                 tokens" )
      | _ ->
          failwith "Missing semicolon after condition in for-loop declaration"
      )
  | _ ->
      failwith "Wrong call to loop_stmt: not an loop statement"

and block_stmt_inner tokens acc =
  match tokens with
  | [] ->
      error "Block statement" "block statement" tokens
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
      error "Block statement" "block statement" rest

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
      if_stmt t |> make_result_fixme
  | Lex.While :: _ as t ->
      while_stmt t |> make_result_fixme
  | Lex.For :: _ as t ->
      for_stmt t |> make_result_fixme
  | _ as t ->
      let e, rest = expression_stmt t in
      (Expr e, rest) |> make_result_fixme

and var_decl :
       Lex.lex_token list
    -> (statement * Lex.lex_token list, string * Lex.lex_token list) result =
  function
  | Lex.Var :: Lex.Identifier n :: Lex.Equal :: rest -> (
      let e, rest = expression rest in
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
