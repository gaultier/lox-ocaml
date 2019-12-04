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

type tokens = token list[@@deriving sexp_of]

type ctx = {
  source : string;
  current_line : int;
  current_column : int;
  current_pos : int;
  rest : char list;
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
    match ctx.rest with
    | '"' :: rest ->
        {
          ctx with
          rest;
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
        }
    | _ -> failwith "Wrong call to lex_string"
  in
  let rec lex_string_rec len ctx =
    match ctx.rest with
    | [] ->
        {
          ctx with
          current_column = ctx.current_column + len;
          current_pos = ctx.current_pos + len;
          rest = [];
          tokens =
            Result.failf
              "Missing closing quote, no more tokens for string: `%s`"
              (String.sub ~pos:ctx.current_pos ~len ctx.source |> String.rstrip)
            :: ctx.tokens;
        }
    | '\n' :: rest ->
        lex_string_rec (len + 1)
          { ctx with current_line = ctx.current_line + 1; rest }
    | '"' :: rest ->
        {
          ctx with
          current_column = ctx.current_column + len + 1;
          current_pos = ctx.current_pos + len + 1;
          rest;
          tokens =
            Ok
              {
                kind = String (String.sub ~pos:ctx.current_pos ~len ctx.source);
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
    | _ :: rest -> lex_string_rec (len + 1) { ctx with rest }
  in

  lex_string_rec 0 ctx

let lex_num ctx =
  let digits, rest = List.split_while ctx.rest ~f:Char.is_digit in
  let len = List.length digits in
  match rest with
  | '.' :: ('0' .. '9' :: _ as rest) ->
      let digits_after_dot, rest = List.split_while rest ~f:Char.is_digit in
      let len = len + 1 + List.length digits_after_dot in
      let t =
        Ok
          {
            kind =
              Number
                ( String.sub ctx.source ~pos:ctx.current_pos ~len
                |> Float.of_string );
            lines = ctx.current_line;
            columns = ctx.current_column;
          }
      in
      {
        ctx with
        current_column = ctx.current_column + len;
        current_pos = ctx.current_pos + len;
        tokens = t :: ctx.tokens;
        rest;
      }
  | '.' :: (_ as rest) ->
      let t =
        Error
          (Printf.sprintf "%d:%d:Trailing `.` in number not allowed: `%s`"
             ctx.current_line (ctx.current_column + len)
             (String.sub ctx.source ~pos:ctx.current_pos ~len:(len+1)))
      in
      {
        ctx with
        current_column = ctx.current_column + len + 1;
        current_pos = ctx.current_pos + len + 1;
        tokens = t :: ctx.tokens;
        rest;
      }
  | _ ->
      let t =
        Ok
          {
            kind = Number ( String.sub ctx.source ~pos:ctx.current_pos ~len|> Float.of_string);
            lines = ctx.current_line;
            columns = ctx.current_column;
          }
      in
      {
        ctx with
        current_column = ctx.current_column + len;
        current_pos = ctx.current_pos + len;
        tokens = t :: ctx.tokens;
        rest;
      }

let lex_identifier ctx =
  let identifier, rest = List.split_while ctx.rest ~f:(fun c -> Char.is_alphanum c || Char.equal c '_') in
  let len = List.length identifier in
  let s = String.sub ctx.source ~pos:ctx.current_pos ~len in
  let k = match Map.find keywords s with Some k -> k | _ -> Identifier s in
  {
    ctx with
    current_column = ctx.current_column + len;
    current_pos = ctx.current_pos + len;
    tokens =
      Ok { kind = k; lines = ctx.current_line; columns = ctx.current_column }
      :: ctx.tokens;
    rest;
  }

let rec lex_r ctx =
  match ctx.rest with
  | [] | '\000' :: _ ->  List.rev ctx.tokens
  | '{' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = CurlyBraceLeft;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '}' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = CurlyBraceRight;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '(' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = ParenLeft;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | ')' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = ParenRight;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | ',' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Comma;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '.' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Dot;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '-' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Minus;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '+' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Plus;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | ';' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = SemiColon;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '*' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Star;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '/' :: '/' :: rest ->
      let dropped, rest =
        List.split_while rest ~f:(fun c -> not (Char.equal c '\n'))
      in
      let len = List.length dropped + 2 in
      lex_r
        {
          ctx with
          current_column = ctx.current_column + len;
          current_pos = ctx.current_pos + len;
          rest;
        }
  | '/' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Slash;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '!' :: '=' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 2;
          current_pos = ctx.current_pos + 2;
          rest;
          tokens =
            Ok
              {
                kind = BangEqual;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '!' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 2;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Bang;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '=' :: '=' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 2;
          current_pos = ctx.current_pos + 2;
          rest;
          tokens =
            Ok
              {
                kind = EqualEqual;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '=' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Equal;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '<' :: '=' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 2;
          current_pos = ctx.current_pos + 2;
          rest;
          tokens =
            Ok
              {
                kind = LessEqual;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '<' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Less;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '>' :: '=' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 2;
          current_pos = ctx.current_pos + 2;
          rest;
          tokens =
            Ok
              {
                kind = GreaterEqual;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | '>' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens =
            Ok
              {
                kind = Greater;
                lines = ctx.current_line;
                columns = ctx.current_column;
              }
            :: ctx.tokens;
        }
  | ' ' :: rest | '\t' :: rest | '\r' :: rest ->
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
        }
  | '\n' :: rest ->
      lex_r
        {
          ctx with
          current_line = ctx.current_line + 1;
          current_column = 1;
          current_pos = ctx.current_pos + 1;
          rest;
        }
  | '"' :: _ -> ctx |> lex_string |> lex_r
  | '0' .. '9' :: _ -> ctx |> lex_num |> lex_r
  | x :: _ when Char.is_alpha x -> ctx |> lex_identifier |> lex_r
  | x :: rest ->
      let err =
        Result.failf "%d:%d:Unkown token: `%c`" ctx.current_line
          ctx.current_column x
      in
      lex_r
        {
          ctx with
          current_column = ctx.current_column + 1;
          current_pos = ctx.current_pos + 1;
          rest;
          tokens = err :: ctx.tokens;
        }

let lex s =
  lex_r
    {
      source = s;
      current_line = 1;
      current_column = 1;
      current_pos = 0;
      rest = String.to_list s;
      tokens = [];
    }
  |>  Result.combine_errors

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
