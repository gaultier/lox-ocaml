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

type token = {kind: token_kind; lines: int; columns: int}

let keywords =
  Base.Hashtbl.of_alist_exn
    (module Base.String)
    [ ("and", And)
    ; ("class", Class)
    ; ("else", Else)
    ; ("false", False)
    ; ("for", For)
    ; ("fun", Fun)
    ; ("if", If)
    ; ("nil", Nil)
    ; ("or", Or)
    ; ("print", Print)
    ; ("return", Return)
    ; ("super", Super)
    ; ("this", This)
    ; ("true", True)
    ; ("var", Var)
    ; ("while", While) ]

let rec string_count_lines_columns lines columns = function
  | [] ->
      (lines, columns)
  | '\n' :: rest ->
      (string_count_lines_columns [@tailcall]) (lines + 1) 1 rest
  | _ :: rest ->
      (string_count_lines_columns [@tailcall]) lines (columns + 1) rest

let lex_string rest lines columns =
  let sl, rest = Base.List.split_while rest ~f:(fun c -> c != '"') in
  let s = Base.String.of_char_list sl in
  match rest with
  | '"' :: rest ->
      let t = {kind= String s; lines; columns} in
      let lines, columns = string_count_lines_columns lines columns sl in
      (Ok t, rest, lines, columns + 1)
  | _ ->
      (* TODO: we should sync at newlines probably here *)
      let lines, columns = string_count_lines_columns lines columns sl in
      ( Base.Result.failf
          "%d:%d:Missing closing quote, no more tokens for string: `%s`" lines
          columns s
      , rest
      , lines
      , columns )

let lex_num rest lines columns =
  (* trailing dot is allowed for now *)
  let digits, rest =
    Base.List.split_while rest ~f:(fun c -> Base.Char.is_digit c || c == '.')
  in
  let s = digits |> Base.String.of_char_list in
  let new_columns = columns + String.length s in
  if Base.String.is_suffix s ~suffix:"." then
    ( Base.Result.failf "%d:%d:Trailing `.` in number not allowed: `%s`" lines
        (new_columns - 1) s
    , rest
    , lines
    , new_columns )
  else
    ( Ok {kind= Number (Float.of_string s); lines; columns}
    , rest
    , lines
    , new_columns )

let lex_identifier rest lines columns =
  let identifier, rest = Base.List.split_while rest ~f:Base.Char.is_alphanum in
  let s = Base.String.of_char_list identifier in
  let new_columns = columns + String.length s in
  match Base.Hashtbl.find keywords s with
  | Some k ->
      (Ok {kind= k; lines; columns}, rest, lines, new_columns)
  | _ ->
      (Ok {kind= Identifier s; lines; columns}, rest, lines, new_columns)

let rec lex_r acc rest lines columns =
  match rest with
  | [] | '\000' :: _ ->
      acc
  | '{' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= CurlyBraceLeft; lines; columns} :: acc)
        rest lines (columns + 1)
  | '}' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= CurlyBraceRight; lines; columns} :: acc)
        rest lines (columns + 1)
  | '(' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= ParenLeft; lines; columns} :: acc)
        rest lines (columns + 1)
  | ')' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= ParenRight; lines; columns} :: acc)
        rest lines (columns + 1)
  | ',' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Comma; lines; columns} :: acc)
        rest lines (columns + 1)
  | '.' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Dot; lines; columns} :: acc)
        rest lines (columns + 1)
  | '-' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Minus; lines; columns} :: acc)
        rest lines (columns + 1)
  | '+' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Plus; lines; columns} :: acc)
        rest lines (columns + 1)
  | ';' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= SemiColon; lines; columns} :: acc)
        rest lines (columns + 1)
  | '*' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Star; lines; columns} :: acc)
        rest lines (columns + 1)
  | '/' :: '/' :: rest ->
      (lex_r [@tailcall]) acc
        (Base.List.drop_while rest ~f:(fun c -> c != '\n'))
        lines (columns + 1)
  | '/' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Slash; lines; columns} :: acc)
        rest lines (columns + 1)
  | '!' :: '=' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= BangEqual; lines; columns} :: acc)
        rest lines (columns + 2)
  | '!' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Bang; lines; columns} :: acc)
        rest lines (columns + 1)
  | '=' :: '=' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= EqualEqual; lines; columns} :: acc)
        rest lines (columns + 2)
  | '=' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Equal; lines; columns} :: acc)
        rest lines (columns + 1)
  | '<' :: '=' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= LessEqual; lines; columns} :: acc)
        rest lines (columns + 2)
  | '<' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Less; lines; columns} :: acc)
        rest lines (columns + 1)
  | '>' :: '=' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= GreaterEqual; lines; columns} :: acc)
        rest lines (columns + 2)
  | '>' :: rest ->
      (lex_r [@tailcall])
        (Ok {kind= Greater; lines; columns} :: acc)
        rest lines (columns + 1)
  | ' ' :: rest | '\t' :: rest | '\r' :: rest ->
      (lex_r [@tailcall]) acc rest lines (columns + 1)
  | '\n' :: rest ->
      (lex_r [@tailcall]) acc rest (lines + 1) 1
  | '"' :: rest ->
      let columns = columns + 1 in
      let t, rest, lines, columns = lex_string rest lines columns in
      (lex_r [@tailcall]) (t :: acc) rest lines columns
  | '0' .. '9' :: _ ->
      let t, rest, lines, columns = lex_num rest lines columns in
      (lex_r [@tailcall]) (t :: acc) rest lines columns
  | x :: _ when Base.Char.is_alpha x ->
      let t, rest, lines, columns = lex_identifier rest lines columns in
      (lex_r [@tailcall]) (t :: acc) rest lines columns
  | x :: rest ->
      let err = Base.Result.failf "%d:%d:Unkown token: `%c`" lines columns x in
      (lex_r [@tailcall]) (err :: acc) rest lines (columns + 1)

let lex s =
  lex_r [] (Base.String.to_list s) 1 1
  |> List.rev |> Base.Result.combine_errors

let token_to_string = function
  | CurlyBraceLeft ->
      "{"
  | CurlyBraceRight ->
      "}"
  | ParenLeft ->
      "("
  | ParenRight ->
      ")"
  | Comma ->
      ","
  | Dot ->
      "."
  | Minus ->
      "-"
  | Plus ->
      "+"
  | SemiColon ->
      ";"
  | Star ->
      "*"
  | Slash ->
      "/"
  | Bang ->
      "!"
  | BangEqual ->
      "!="
  | Equal ->
      "="
  | EqualEqual ->
      "=="
  | Less ->
      "<"
  | LessEqual ->
      "<="
  | Greater ->
      ">"
  | GreaterEqual ->
      ">="
  | And ->
      "and"
  | Class ->
      "class"
  | Else ->
      "else"
  | False ->
      "false"
  | For ->
      "for"
  | Fun ->
      "fun"
  | If ->
      "if"
  | Nil ->
      "nil"
  | Or ->
      "or"
  | Print ->
      "print"
  | Return ->
      "return"
  | Super ->
      "super"
  | This ->
      "this"
  | True ->
      "true"
  | Var ->
      "var"
  | While ->
      "while"
  | String s ->
      Printf.sprintf "\"%s\"" s
  | Number f ->
      Float.to_string f
  | Identifier i ->
      i
