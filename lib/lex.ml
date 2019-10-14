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

let lex_string acc rest =
  let s, rest = Base.List.split_while rest ~f:(fun c -> c != '"') in
  match rest with
  | '"' :: rest ->
      (String (Base.String.of_char_list s) :: acc, rest)
  | _ ->
      failwith "Missing closing quote, no more tokens"

let rec lex_r acc rest =
  match rest with
  | [] ->
      acc
  | '{' :: rest ->
      (lex_r [@tailcall]) (CurlyBraceLeft :: acc) rest
  | '}' :: rest ->
      (lex_r [@tailcall]) (CurlyBraceRight :: acc) rest
  | '(' :: rest ->
      (lex_r [@tailcall]) (ParenLeft :: acc) rest
  | ')' :: rest ->
      (lex_r [@tailcall]) (ParenRight :: acc) rest
  | ',' :: rest ->
      (lex_r [@tailcall]) (Comma :: acc) rest
  | '.' :: rest ->
      (lex_r [@tailcall]) (Dot :: acc) rest
  | '-' :: rest ->
      (lex_r [@tailcall]) (Minus :: acc) rest
  | '+' :: rest ->
      (lex_r [@tailcall]) (Plus :: acc) rest
  | ';' :: rest ->
      (lex_r [@tailcall]) (SemiColon :: acc) rest
  | '*' :: rest ->
      (lex_r [@tailcall]) (Star :: acc) rest
  | '/' :: '/' :: rest ->
      (lex_r [@tailcall]) acc
        (Base.List.drop_while rest ~f:(fun c -> c != '\n'))
  | '/' :: rest ->
      (lex_r [@tailcall]) (Slash :: acc) rest
  | '!' :: '=' :: rest ->
      (lex_r [@tailcall]) (BangEqual :: acc) rest
  | '!' :: rest ->
      (lex_r [@tailcall]) (Bang :: acc) rest
  | '=' :: '=' :: rest ->
      (lex_r [@tailcall]) (EqualEqual :: acc) rest
  | '=' :: rest ->
      (lex_r [@tailcall]) (Equal :: acc) rest
  | '<' :: '=' :: rest ->
      (lex_r [@tailcall]) (LessEqual :: acc) rest
  | '<' :: rest ->
      (lex_r [@tailcall]) (Less :: acc) rest
  | '>' :: '=' :: rest ->
      (lex_r [@tailcall]) (GreaterEqual :: acc) rest
  | '>' :: rest ->
      (lex_r [@tailcall]) (Greater :: acc) rest
  | ' ' :: rest | '\n' :: rest | '\t' :: rest | '\r' :: rest ->
      (lex_r [@tailcall]) acc rest
  | '"' :: rest ->
      let acc, rest = lex_string acc rest in
      (lex_r [@tailcall]) acc rest
  | '0' .. '9' :: _ ->
      (* trailing dot is allowed for now *)
      let digits, r =
        Base.List.split_while rest ~f:(fun c ->
            Base.Char.is_digit c || c == '.')
      in
      let f = digits |> Base.String.of_char_list |> Float.of_string in
      (lex_r [@tailcall]) (Number f :: acc) r
  | x :: _ when Base.Char.is_alpha x -> (
      let identifier, r =
        Base.List.split_while rest ~f:Base.Char.is_alphanum
      in
      let s = Base.String.of_char_list identifier in
      match Base.Hashtbl.find keywords s with
      | Some k ->
          (lex_r [@tailcall]) (k :: acc) r
      | _ ->
          (lex_r [@tailcall]) (Identifier s :: acc) r )
  | '\000' :: _ ->
      acc
  | x :: _ ->
      failwith (Format.sprintf "Unkown token: `%c`" x)

let lex s = lex_r [] (Base.String.to_list s) |> List.rev
