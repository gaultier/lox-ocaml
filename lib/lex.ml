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
  current_line : int;
  current_column : int;
  current_pos : int;
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

let lex_string ctx =
  let ctx =
    match ctx.source.[ctx.current_pos] with
    | '"' ->
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
        }
    | _ -> failwith "Wrong call to lex_string"
  in
  let start_ctx = ctx in
  let rec lex_string_rec ctx =
    match ctx.source.[ctx.current_pos] with
    | exception Invalid_argument _ ->
        {
          ctx with
          tokens =
            Result.failf
              "Missing closing quote, no more tokens for string: `%s`"
              ( String.sub ~pos:start_ctx.current_pos
                  ~len:(ctx.current_pos - start_ctx.current_pos)
                  ctx.source
              |> String.rstrip )
            :: ctx.tokens;
        }
    | '\n' ->
        lex_string_rec
          {
            ctx with
            current_line = ctx.current_line + 1;
            current_pos = ctx.current_pos + 1;
            current_column = 0;
          }
    | '"' ->
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind =
                  String
                    (String.sub ~pos:start_ctx.current_pos
                       ~len:(ctx.current_pos - start_ctx.current_pos)
                       ctx.source);
                lines = start_ctx.current_line;
                columns = start_ctx.current_column;
              }
            :: ctx.tokens;
        }
    | _ ->
        lex_string_rec
          {
            ctx with
            current_pos = ctx.current_pos + 1;
            current_column = ctx.current_column + 1;
          }
  in

  lex_string_rec ctx

let lex_num ctx =
  let rec many_digits ctx =
    match ctx.source.[ctx.current_pos] with
    | '0' .. '9' ->
        many_digits
          {
            ctx with
            current_pos = ctx.current_pos + 1;
            current_column = ctx.current_column + 1;
          }
    | _ -> ctx
  in

  let start = ctx.current_pos in
  let ctx = many_digits ctx in
  match ctx.source.[ctx.current_pos] with
  | '.' ->
      let ctx =
        {
          ctx with
          current_pos = ctx.current_pos + 1;
          current_column = ctx.current_column;
        }
      in
      let ctx = many_digits ctx in
      let len = ctx.current_pos - start in
      let t =
        Ok
          {
            kind =
              Number (String.sub ctx.source ~pos:start ~len |> Float.of_string);
            lines = ctx.current_line;
            columns = ctx.current_column;
          }
      in
      {
        ctx with
        current_column = ctx.current_column + len;
        current_pos = ctx.current_pos + len;
        tokens = t :: ctx.tokens;
      }
  | _ ->
      let len = ctx.current_pos - start in
      let t =
        Ok
          {
            kind =
              Number (String.sub ctx.source ~pos:start ~len |> Float.of_string);
            lines = ctx.current_line;
            columns = ctx.current_column;
          }
      in
      {
        ctx with
        current_column = ctx.current_column + len;
        current_pos = ctx.current_pos + len;
        tokens = t :: ctx.tokens;
      }

let lex_identifier ctx =
  let rec zero_or_many_alphanum ctx =
    match ctx.source.[ctx.current_pos] with
    | 'a' .. 'z' | 'A' .. 'Z' | '_' ->
        zero_or_many_alphanum
          {
            ctx with
            current_pos = ctx.current_pos + 1;
            current_column = ctx.current_column + 1;
          }
    | _ -> ctx
  in
  let one_alpha ctx =
    match ctx.source.[ctx.current_pos] with
    | 'a' .. 'z' ->
        {
          ctx with
          current_pos = ctx.current_pos + 1;
          current_column = ctx.current_column + 1;
        }
    | _ -> ctx
  in

  let start = ctx.current_pos in
  let ctx = one_alpha ctx |> zero_or_many_alphanum in
  let len = ctx.current_pos - start in
  let s = String.sub ctx.source ~pos:ctx.current_pos ~len in
  let k = match Map.find keywords s with Some k -> k | _ -> Identifier s in
  {
    ctx with
    current_column = ctx.current_column + len;
    current_pos = ctx.current_pos + len;
    tokens =
      Ok { kind = k; lines = ctx.current_line; columns = ctx.current_column }
      :: ctx.tokens;
  }

let rec lex_r ctx =
  match ctx.source.[ctx.current_pos] with
  | exception Invalid_argument _ -> ctx.tokens
  | '{' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = CurlyBraceLeft;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '}' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = CurlyBraceRight;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '(' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = ParenLeft;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | ')' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = ParenRight;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | ',' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = Comma;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '.' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = Dot;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '-' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = Minus;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '+' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = Plus;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | ';' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = SemiColon;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '*' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens =
            Ok
              {
                kind = Star;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '/' -> (
      match ctx.source.[ctx.current_pos + 1] with
      | '/' ->
          let rec until_newline ctx =
            match ctx.source.[ctx.current_pos] with
            | '\n' -> ctx
            | _ -> until_newline { ctx with current_pos = ctx.current_pos + 1 }
          in
          let start = ctx.current_pos in
          let ctx = until_newline ctx in
          let len = ctx.current_pos - start + 2 in
          lex_r
            {
              ctx with
              current_column = ctx.current_column + len;
              current_pos = ctx.current_pos + len;
            }
      | _ ->
          lex_r
            {
              ctx with
              current_column = ctx.current_column + 1;
              current_pos = ctx.current_pos + 1;
              tokens =
                Ok
                  {
                    kind = Slash;
                    lines = ctx.current_line;
                    columns = ctx.current_column;
                  }
                :: ctx.tokens;
            } )
  | '!' -> (
      match ctx.source.[ctx.current_pos] with
      | '=' ->
          lex_r
            {
              ctx with
              current_column = ctx.current_column + 2;
              current_pos = ctx.current_pos + 2;
              tokens =
                Ok
                  {
                    kind = BangEqual;
                    lines = ctx.current_line;
                    columns = ctx.current_column;
                  }
                :: ctx.tokens;
            }
      | _ ->
          lex_r
            {
              ctx with
              current_column = ctx.current_column + 2;
              current_pos = ctx.current_pos + 1;
              tokens =
                Ok
                  {
                    kind = Bang;
                    lines = ctx.current_line;
                    columns = ctx.current_column;
                  }
                :: ctx.tokens;
            } )
  | '=' -> (
      match ctx.source.[ctx.current_pos + 1] with
      | '=' ->
          lex_r
            {
              ctx with
              current_column = ctx.current_column + 2;
              current_pos = ctx.current_pos + 2;
              tokens =
                Ok
                  {
                    kind = EqualEqual;
                    lines = ctx.current_line;
                    columns = ctx.current_column;
                  }
                :: ctx.tokens;
            }
      | _ ->
          lex_r
            {
              ctx with
              current_column = ctx.current_column + 1;
              current_pos = ctx.current_pos + 1;
              tokens =
                Ok
                  {
                    kind = Equal;
                    lines = ctx.current_line;
                    columns = ctx.current_column;
                  }
                :: ctx.tokens;
            } )
  | '<' -> (
      match ctx.source.[ctx.current_pos + 1] with
      | '=' ->
          lex_r
            {
              ctx with
              current_column = ctx.current_column + 2;
              current_pos = ctx.current_pos + 2;
              tokens =
                Ok
                  {
                    kind = LessEqual;
                    lines = ctx.current_line;
                    columns = ctx.current_column;
                  }
                :: ctx.tokens;
            }
      | _ ->
          lex_r
            {
              ctx with
              current_column = ctx.current_column + 1;
              current_pos = ctx.current_pos + 1;
              tokens =
                Ok
                  {
                    kind = Less;
                    lines = ctx.current_line;
                    columns = ctx.current_column;
                  }
                :: ctx.tokens;
            } )
  | '>' -> (
      match ctx.source.[ctx.current_pos + 1] with
      | '=' ->
          lex_r
            {
              ctx with
              current_column = ctx.current_column + 2;
              current_pos = ctx.current_pos + 2;
              tokens =
                Ok
                  {
                    kind = GreaterEqual;
                    lines = ctx.current_line;
                    columns = ctx.current_column;
                  }
                :: ctx.tokens;
            }
      | _ ->
          lex_r
            {
              ctx with
              current_column = ctx.current_column + 1;
              current_pos = ctx.current_pos + 1;
              tokens =
                Ok
                  {
                    kind = Greater;
                    lines = ctx.current_line;
                    columns = ctx.current_column;
                  }
                :: ctx.tokens;
            } )
  | ' ' | '\t' | '\r' ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
        }
  | '\n' ->
      lex_r
        {
          ctx with
          current_line = ctx.current_line + 1;
          current_column = 1;
          current_pos = ctx.current_pos + 1;
        }
  | '"' -> ctx |> lex_string |> lex_r
  | '0' .. '9' -> ctx |> lex_num |> lex_r
  | x when Char.is_alpha x -> ctx |> lex_identifier |> lex_r
  | x ->
      let err =
        Result.failf "%d:%d:Unkown token: `%c`" ctx.current_line
          ctx.current_column x
      in
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          tokens = err :: ctx.tokens;
        }

let lex s =
  lex_r
    {
      source = s;
      current_line = 1;
      current_column = 1;
      current_pos = 0;
      tokens = [];
    }
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
