open Sexplib.Std

type lex_token =
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
[@@deriving sexp]

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
  let f = digits |> Base.String.of_char_list |> Float.of_string in
  (Ok (Number f), rest)

let lex_identifier rest =
  let identifier, rest = Base.List.split_while rest ~f:Base.Char.is_alphanum in
  let s = Base.String.of_char_list identifier in
  match Base.Hashtbl.find keywords s with
  | Some k ->
      (Ok k, rest)
  | _ ->
      (Ok (Identifier s), rest)

let rec lex_r acc rest =
  match rest with
  | [] | '\000' :: _ ->
      acc
  | '{' :: rest ->
      (lex_r [@tailcall]) (Ok CurlyBraceLeft :: acc) rest
  | '}' :: rest ->
      (lex_r [@tailcall]) (Ok CurlyBraceRight :: acc) rest
  | '(' :: rest ->
      (lex_r [@tailcall]) (Ok ParenLeft :: acc) rest
  | ')' :: rest ->
      (lex_r [@tailcall]) (Ok ParenRight :: acc) rest
  | ',' :: rest ->
      (lex_r [@tailcall]) (Ok Comma :: acc) rest
  | '.' :: rest ->
      (lex_r [@tailcall]) (Ok Dot :: acc) rest
  | '-' :: rest ->
      (lex_r [@tailcall]) (Ok Minus :: acc) rest
  | '+' :: rest ->
      (lex_r [@tailcall]) (Ok Plus :: acc) rest
  | ';' :: rest ->
      (lex_r [@tailcall]) (Ok SemiColon :: acc) rest
  | '*' :: rest ->
      (lex_r [@tailcall]) (Ok Star :: acc) rest
  | '/' :: '/' :: rest ->
      (lex_r [@tailcall]) acc
        (Base.List.drop_while rest ~f:(fun c -> c != '\n'))
  | '/' :: rest ->
      (lex_r [@tailcall]) (Ok Slash :: acc) rest
  | '!' :: '=' :: rest ->
      (lex_r [@tailcall]) (Ok BangEqual :: acc) rest
  | '!' :: rest ->
      (lex_r [@tailcall]) (Ok Bang :: acc) rest
  | '=' :: '=' :: rest ->
      (lex_r [@tailcall]) (Ok EqualEqual :: acc) rest
  | '=' :: rest ->
      (lex_r [@tailcall]) (Ok Equal :: acc) rest
  | '<' :: '=' :: rest ->
      (lex_r [@tailcall]) (Ok LessEqual :: acc) rest
  | '<' :: rest ->
      (lex_r [@tailcall]) (Ok Less :: acc) rest
  | '>' :: '=' :: rest ->
      (lex_r [@tailcall]) (Ok GreaterEqual :: acc) rest
  | '>' :: rest ->
      (lex_r [@tailcall]) (Ok Greater :: acc) rest
  | ' ' :: rest | '\n' :: rest | '\t' :: rest | '\r' :: rest ->
      (lex_r [@tailcall]) acc rest
  | '"' :: rest ->
      let t, rest = lex_string rest in
      (lex_r [@tailcall]) (t :: acc) rest
  | '0' .. '9' :: _ ->
      let t, rest = lex_num rest in
      (lex_r [@tailcall]) (t :: acc) rest
  | x :: _ when Base.Char.is_alpha x ->
      let t, rest = lex_identifier rest in
      (lex_r [@tailcall]) (t :: acc) rest
  | x :: rest ->
      let err = Base.Result.failf "Unkown token: `%c`" x in
      (lex_r [@tailcall]) (err :: acc) rest

let lex s =
  lex_r [] (Base.String.to_list s) |> List.rev |> Base.Result.combine_errors
