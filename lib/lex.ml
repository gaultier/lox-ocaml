open Base

type token_kind =
  | CurlyBraceLeft
  | CurlyBraceRight
  | ParenLeft
  | ParenRight
  | Comma
  | Dot
  | Minus
  | Plus
  | SemiColon
  | Star
  | Slash
  | Bang
  | BangEqual
  | Equal
  | EqualEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | And
  | Class
  | Else
  | False
  | For
  | Fun
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | String of string
  | Number of float
  | Identifier of string
[@@deriving sexp_of]

type token = { kind : token_kind; lines : int; columns : int }
[@@deriving sexp_of]

type tokens = token list [@@deriving sexp_of]

type ctx = {
  source : string;
  line : int;
  column : int;
  pos : int;
  tokens : (token, string) Base.Result.t list;
}

let keywords =
  Map.of_alist_exn
    (module String)
    [
      ("and", And);
      ("class", Class);
      ("else", Else);
      ("false", False);
      ("for", For);
      ("fun", Fun);
      ("if", If);
      ("nil", Nil);
      ("or", Or);
      ("print", Print);
      ("return", Return);
      ("super", Super);
      ("this", This);
      ("true", True);
      ("var", Var);
      ("while", While);
    ]

let peek s i = if i < String.length s then Some s.[i] else None

let next ctx =
  match peek ctx.source ctx.pos with
  | Some '\n' as c ->
      (c, { ctx with pos = ctx.pos + 1; line = ctx.line + 1; column = 0 })
  | Some _ as c -> (c, { ctx with pos = ctx.pos + 1; column = ctx.column + 1 })
  | None -> (None, ctx)

let expect ctx expected =
  let c, ctx = next ctx in
  match c with
  | Some c when Char.equal expected c -> ctx
  | Some c ->
      Printf.failwithf "Expected character `%c`, got: `%c`" expected c ()
  | None ->
      Printf.failwithf "Expected character `%c`, got: no more tokens" expected
        ()

(* let previous ctx = Option.value_exn (peek ctx.source ctx.pos) *)

let lex_string ctx =
  let ctx = expect ctx '"' in
  let start_ctx = ctx in
  let rec lex_string_rec ctx =
    let c, ctx = next ctx in
    match c with
    | None ->
        {
          ctx with
          tokens =
            Result.failf
              "Missing closing quote, no more tokens for string: `%s`"
              ( String.sub ~pos:start_ctx.pos ~len:(ctx.pos - start_ctx.pos)
                  ctx.source
              |> String.rstrip )
            :: ctx.tokens;
        }
    | Some '"' ->
        let t =
          {
            kind =
              String
                (String.sub ~pos:start_ctx.pos
                   ~len:(ctx.pos - start_ctx.pos - 1)
                   ctx.source);
            lines = start_ctx.line;
            columns = start_ctx.column;
          }
        in
        { ctx with tokens = Ok t :: ctx.tokens }
    | _ -> lex_string_rec ctx
  in

  lex_string_rec ctx

let lex_num ctx =
  let rec many_digits ctx =
    match peek ctx.source ctx.pos with
    | Some '0' .. '9' ->
        let _, ctx = next ctx in
        many_digits ctx
    | _ -> ctx
  in

  let start_ctx = ctx in
  let ctx = many_digits ctx in
  let ctx =
    match (peek ctx.source ctx.pos, peek ctx.source (ctx.pos + 1)) with
    | Some '.', Some '0' .. '9' ->
        let _, ctx = next ctx in
        many_digits ctx
    | _ -> ctx
  in
  let len = ctx.pos - start_ctx.pos in
  let t =
    Ok
      {
        kind =
          Number
            (String.sub ctx.source ~pos:start_ctx.pos ~len |> Float.of_string);
        lines = start_ctx.line;
        columns = start_ctx.column;
      }
  in
  { ctx with tokens = t :: ctx.tokens }

let lex_identifier ctx =
  let rec zero_or_many_alphanum ctx =
    match ctx.source.[ctx.pos] with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
        zero_or_many_alphanum
          { ctx with pos = ctx.pos + 1; column = ctx.column + 1 }
    | (exception Invalid_argument _) | _ -> ctx
  in
  let one_alpha ctx =
    match ctx.source.[ctx.pos] with
    | 'a' .. 'z' | 'A' .. 'Z' ->
        { ctx with pos = ctx.pos + 1; column = ctx.column + 1 }
    | (exception Invalid_argument _) | _ -> ctx
  in

  let start_ctx = ctx in
  let ctx = one_alpha ctx |> zero_or_many_alphanum in
  let len = ctx.pos - start_ctx.pos in
  let s = String.sub ctx.source ~pos:start_ctx.pos ~len in
  let k = match Map.find keywords s with Some k -> k | _ -> Identifier s in
  {
    ctx with
    tokens =
      Ok { kind = k; lines = start_ctx.line; columns = start_ctx.column }
      :: ctx.tokens;
  }

let rec lex_r ctx =
  match ctx.source.[ctx.pos] with
  | exception Invalid_argument _ -> List.rev ctx.tokens
  | '{' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok { kind = CurlyBraceLeft; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | '}' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok
              { kind = CurlyBraceRight; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | '(' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok { kind = ParenLeft; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | ')' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok { kind = ParenRight; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | ',' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok { kind = Comma; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | '.' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok { kind = Dot; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | '-' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok { kind = Minus; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | '+' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok { kind = Plus; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | ';' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok { kind = SemiColon; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | '*' ->
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens =
            Ok { kind = Star; lines = ctx.line; columns = ctx.column }
            :: ctx.tokens;
        }
  | '/' -> (
      match ctx.source.[ctx.pos + 1] with
      | '/' ->
          let ctx = { ctx with pos = ctx.pos + 2; column = ctx.column + 2 } in
          let rec until_newline ctx =
            match ctx.source.[ctx.pos] with
            | '\n' | (exception Invalid_argument _) ->
                { ctx with line = ctx.line + 1; column = 1; pos = ctx.pos + 1 }
            | _ ->
                until_newline
                  { ctx with pos = ctx.pos + 1; column = ctx.column + 1 }
          in
          until_newline ctx |> lex_r
      | (exception Invalid_argument _) | _ ->
          lex_r
            {
              ctx with
              column = ctx.column + 1;
              pos = ctx.pos + 1;
              tokens =
                Ok { kind = Slash; lines = ctx.line; columns = ctx.column }
                :: ctx.tokens;
            } )
  | '!' -> (
      match ctx.source.[ctx.pos + 1] with
      | '=' ->
          lex_r
            {
              ctx with
              column = ctx.column + 2;
              pos = ctx.pos + 2;
              tokens =
                Ok { kind = BangEqual; lines = ctx.line; columns = ctx.column }
                :: ctx.tokens;
            }
      | (exception Invalid_argument _) | _ ->
          lex_r
            {
              ctx with
              column = ctx.column + 2;
              pos = ctx.pos + 1;
              tokens =
                Ok { kind = Bang; lines = ctx.line; columns = ctx.column }
                :: ctx.tokens;
            } )
  | '=' -> (
      match ctx.source.[ctx.pos + 1] with
      | '=' ->
          lex_r
            {
              ctx with
              column = ctx.column + 2;
              pos = ctx.pos + 2;
              tokens =
                Ok { kind = EqualEqual; lines = ctx.line; columns = ctx.column }
                :: ctx.tokens;
            }
      | (exception Invalid_argument _) | _ ->
          lex_r
            {
              ctx with
              column = ctx.column + 1;
              pos = ctx.pos + 1;
              tokens =
                Ok { kind = Equal; lines = ctx.line; columns = ctx.column }
                :: ctx.tokens;
            } )
  | '<' -> (
      match ctx.source.[ctx.pos + 1] with
      | '=' ->
          lex_r
            {
              ctx with
              column = ctx.column + 2;
              pos = ctx.pos + 2;
              tokens =
                Ok { kind = LessEqual; lines = ctx.line; columns = ctx.column }
                :: ctx.tokens;
            }
      | (exception Invalid_argument _) | _ ->
          lex_r
            {
              ctx with
              column = ctx.column + 1;
              pos = ctx.pos + 1;
              tokens =
                Ok { kind = Less; lines = ctx.line; columns = ctx.column }
                :: ctx.tokens;
            } )
  | '>' -> (
      match ctx.source.[ctx.pos + 1] with
      | '=' ->
          lex_r
            {
              ctx with
              column = ctx.column + 2;
              pos = ctx.pos + 2;
              tokens =
                Ok
                  {
                    kind = GreaterEqual;
                    lines = ctx.line;
                    columns = ctx.column;
                  }
                :: ctx.tokens;
            }
      | (exception Invalid_argument _) | _ ->
          lex_r
            {
              ctx with
              column = ctx.column + 1;
              pos = ctx.pos + 1;
              tokens =
                Ok { kind = Greater; lines = ctx.line; columns = ctx.column }
                :: ctx.tokens;
            } )
  | ' ' | '\t' | '\r' ->
      lex_r { ctx with column = ctx.column + 1; pos = ctx.pos + 1 }
  | '\n' ->
      lex_r { ctx with line = ctx.line + 1; column = 1; pos = ctx.pos + 1 }
  | '"' -> ctx |> lex_string |> lex_r
  | '0' .. '9' -> ctx |> lex_num |> lex_r
  | x when Char.is_alpha x -> ctx |> lex_identifier |> lex_r
  | x ->
      let err = Result.failf "%d:%d:Unkown token: `%c`" ctx.line ctx.column x in
      lex_r
        {
          ctx with
          column = ctx.column + 1;
          pos = ctx.pos + 1;
          tokens = err :: ctx.tokens;
        }

let lex s =
  lex_r { source = s; line = 1; column = 1; pos = 0; tokens = [] }
  |> Result.combine_errors

let token_to_string = function
  | CurlyBraceLeft -> "{"
  | CurlyBraceRight -> "}"
  | ParenLeft -> "("
  | ParenRight -> ")"
  | Comma -> ","
  | Dot -> "."
  | Minus -> "-"
  | Plus -> "+"
  | SemiColon -> ";"
  | Star -> "*"
  | Slash -> "/"
  | Bang -> "!"
  | BangEqual -> "!="
  | Equal -> "="
  | EqualEqual -> "=="
  | Less -> "<"
  | LessEqual -> "<="
  | Greater -> ">"
  | GreaterEqual -> ">="
  | And -> "and"
  | Class -> "class"
  | Else -> "else"
  | False -> "false"
  | For -> "for"
  | Fun -> "fun"
  | If -> "if"
  | Nil -> "nil"
  | Or -> "or"
  | Print -> "print"
  | Return -> "return"
  | Super -> "super"
  | This -> "this"
  | True -> "true"
  | Var -> "var"
  | While -> "while"
  | String s -> Printf.sprintf "\"%s\"" s
  | Number f -> Float.to_string f
  | Identifier i -> i
