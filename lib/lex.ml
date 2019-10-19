type token =
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

let lex_string rest =
  let s, rest = Base.List.split_while rest ~f:(fun c -> c != '"') in
  match rest with
  | '"' :: rest ->
      (Ok (String (Base.String.of_char_list s)), rest)
  | _ ->
      (Error "Missing closing quote, no more tokens", rest)

let lex_num rest =
  (* trailing dot is allowed for now *)
  let digits, rest =
    Base.List.split_while rest ~f:(fun c -> Base.Char.is_digit c || c == '.')
  in
  let s = digits |> Base.String.of_char_list in
  if Base.String.is_suffix s ~suffix:"." then
    (Base.Result.failf "Trailing `.` in number not allowed: `%s`" s, rest)
  else (Ok (Number (Float.of_string s)), rest)

let lex_identifier rest =
  let identifier, rest = Base.List.split_while rest ~f:Base.Char.is_alphanum in
  let s = Base.String.of_char_list identifier in
  match Base.Hashtbl.find keywords s with
  | Some k ->
      (Ok k, rest, 0, String.length s)
  | _ ->
      (Ok (Identifier s), rest, 0, String.length s)

let rec lex_r acc rest lines columns =
  match rest with
  | [] | '\000' :: _ ->
      acc
  | '{' :: rest ->
      (lex_r [@tailcall]) (Ok CurlyBraceLeft :: acc) rest lines (columns + 1)
  | '}' :: rest ->
      (lex_r [@tailcall]) (Ok CurlyBraceRight :: acc) rest lines (columns + 1)
  | '(' :: rest ->
      (lex_r [@tailcall]) (Ok ParenLeft :: acc) rest lines (columns + 1)
  | ')' :: rest ->
      (lex_r [@tailcall]) (Ok ParenRight :: acc) rest lines (columns + 1)
  | ',' :: rest ->
      (lex_r [@tailcall]) (Ok Comma :: acc) rest lines (columns + 1)
  | '.' :: rest ->
      (lex_r [@tailcall]) (Ok Dot :: acc) rest lines (columns + 1)
  | '-' :: rest ->
      (lex_r [@tailcall]) (Ok Minus :: acc) rest lines (columns + 1)
  | '+' :: rest ->
      (lex_r [@tailcall]) (Ok Plus :: acc) rest lines (columns + 1)
  | ';' :: rest ->
      (lex_r [@tailcall]) (Ok SemiColon :: acc) rest lines (columns + 1)
  | '*' :: rest ->
      (lex_r [@tailcall]) (Ok Star :: acc) rest lines (columns + 1)
  | '/' :: '/' :: rest ->
      (lex_r [@tailcall]) acc
        (Base.List.drop_while rest ~f:(fun c -> c != '\n'))
        lines (columns + 1)
  | '/' :: rest ->
      (lex_r [@tailcall]) (Ok Slash :: acc) rest lines (columns + 1)
  | '!' :: '=' :: rest ->
      (lex_r [@tailcall]) (Ok BangEqual :: acc) rest lines (columns + 2)
  | '!' :: rest ->
      (lex_r [@tailcall]) (Ok Bang :: acc) rest lines (columns + 1)
  | '=' :: '=' :: rest ->
      (lex_r [@tailcall]) (Ok EqualEqual :: acc) rest lines (columns + 2)
  | '=' :: rest ->
      (lex_r [@tailcall]) (Ok Equal :: acc) rest lines (columns + 1)
  | '<' :: '=' :: rest ->
      (lex_r [@tailcall]) (Ok LessEqual :: acc) rest lines (columns + 2)
  | '<' :: rest ->
      (lex_r [@tailcall]) (Ok Less :: acc) rest lines (columns + 1)
  | '>' :: '=' :: rest ->
      (lex_r [@tailcall]) (Ok GreaterEqual :: acc) rest lines (columns + 2)
  | '>' :: rest ->
      (lex_r [@tailcall]) (Ok Greater :: acc) rest lines (columns + 1)
  | ' ' :: rest | '\t' :: rest | '\r' :: rest ->
      (lex_r [@tailcall]) acc rest lines (columns + 1)
  | '\n' :: rest ->
      (lex_r [@tailcall]) acc rest (lines + 1) 1
  | '"' :: rest ->
      let t, rest = lex_string rest in
      (lex_r [@tailcall]) (t :: acc) rest lines (columns + 1)
  | '0' .. '9' :: _ ->
      let t, rest = lex_num rest in
      (lex_r [@tailcall]) (t :: acc) rest lines (columns + 1)
  | x :: _ when Base.Char.is_alpha x ->
      let t, rest, dlines, dcolumns = lex_identifier rest in
      (lex_r [@tailcall]) (t :: acc) rest (lines + dlines) (columns + dcolumns)
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
