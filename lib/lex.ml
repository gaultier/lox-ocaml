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

let rec lex_r acc rest =
  match rest with
  | [] ->
      acc
  | '{' :: irest ->
      (lex_r [@tailcall]) (CurlyBraceLeft :: acc) irest
  | '}' :: irest ->
      (lex_r [@tailcall]) (CurlyBraceRight :: acc) irest
  | '(' :: irest ->
      (lex_r [@tailcall]) (ParenLeft :: acc) irest
  | ')' :: irest ->
      (lex_r [@tailcall]) (ParenRight :: acc) irest
  | ',' :: irest ->
      (lex_r [@tailcall]) (Comma :: acc) irest
  | '.' :: irest ->
      (lex_r [@tailcall]) (Dot :: acc) irest
  | '-' :: irest ->
      (lex_r [@tailcall]) (Minus :: acc) irest
  | '+' :: irest ->
      (lex_r [@tailcall]) (Plus :: acc) irest
  | ';' :: irest ->
      (lex_r [@tailcall]) (SemiColon :: acc) irest
  | '*' :: irest ->
      (lex_r [@tailcall]) (Star :: acc) irest
  | '/' :: '/' :: irest ->
      (lex_r [@tailcall]) acc
        (Base.List.drop_while irest ~f:(fun c -> c != '\n'))
  | '/' :: irest ->
      (lex_r [@tailcall]) (Slash :: acc) irest
  | '!' :: '=' :: irest ->
      (lex_r [@tailcall]) (BangEqual :: acc) irest
  | '!' :: irest ->
      (lex_r [@tailcall]) (Bang :: acc) irest
  | '=' :: '=' :: irest ->
      (lex_r [@tailcall]) (EqualEqual :: acc) irest
  | '=' :: irest ->
      (lex_r [@tailcall]) (Equal :: acc) irest
  | '<' :: '=' :: irest ->
      (lex_r [@tailcall]) (LessEqual :: acc) irest
  | '<' :: irest ->
      (lex_r [@tailcall]) (Less :: acc) irest
  | '>' :: '=' :: irest ->
      (lex_r [@tailcall]) (GreaterEqual :: acc) irest
  | '>' :: irest ->
      (lex_r [@tailcall]) (Greater :: acc) irest
  | ' ' :: irest ->
      (lex_r [@tailcall]) acc irest
  | '\n' :: irest ->
      (lex_r [@tailcall]) acc irest
  | '\t' :: irest ->
      (lex_r [@tailcall]) acc irest
  | '\r' :: irest ->
      (lex_r [@tailcall]) acc irest
  | '"' :: irest -> (
      let s, r = Base.List.split_while irest ~f:(fun c -> c != '"') in
      match r with
      | '"' :: rrest ->
          (lex_r [@tailcall])
            (String (Base.String.of_char_list s) :: acc)
            rrest
      | _ ->
          failwith "Missing closing quote, no more tokens" )
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
      failwith ("Unkown token: " ^ String.make 1 x)

let lex s = lex_r [] (Base.String.to_list s) |> List.rev
