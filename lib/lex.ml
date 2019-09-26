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
  | LexString of char list
  | LexNumber of float
  | LexIdentifier of char list
  | Unknown of char list
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
      lex_r (CurlyBraceLeft :: acc) irest
  | '}' :: irest ->
      lex_r (CurlyBraceRight :: acc) irest
  | '(' :: irest ->
      lex_r (ParenLeft :: acc) irest
  | ')' :: irest ->
      lex_r (ParenRight :: acc) irest
  | ',' :: irest ->
      lex_r (Comma :: acc) irest
  | '.' :: irest ->
      lex_r (Dot :: acc) irest
  | '-' :: irest ->
      lex_r (Minus :: acc) irest
  | '+' :: irest ->
      lex_r (Plus :: acc) irest
  | ';' :: irest ->
      lex_r (SemiColon :: acc) irest
  | '*' :: irest ->
      lex_r (Star :: acc) irest
  | '/' :: '/' :: irest ->
      lex_r acc (Base.List.drop_while irest ~f:(fun c -> c != '\n'))
  | '/' :: irest ->
      lex_r (Slash :: acc) irest
  | '!' :: '=' :: irest ->
      lex_r (BangEqual :: acc) irest
  | '!' :: irest ->
      lex_r (Bang :: acc) irest
  | '=' :: '=' :: irest ->
      lex_r (EqualEqual :: acc) irest
  | '=' :: irest ->
      lex_r (Equal :: acc) irest
  | '<' :: '=' :: irest ->
      lex_r (LessEqual :: acc) irest
  | '<' :: irest ->
      lex_r (Less :: acc) irest
  | '>' :: '=' :: irest ->
      lex_r (GreaterEqual :: acc) irest
  | '>' :: irest ->
      lex_r (Greater :: acc) irest
  | ' ' :: irest ->
      lex_r acc irest
  | '\n' :: irest ->
      lex_r acc irest
  | '\t' :: irest ->
      lex_r acc irest
  | '\r' :: irest ->
      lex_r acc irest
  | '"' :: irest -> (
      let s, r = Base.List.split_while irest ~f:(fun c -> c != '"') in
      match r with
      | '"' :: rrest ->
          lex_r (LexString s :: acc) rrest
      | _ ->
          lex_r (Unknown ('"' :: s) :: acc) r )
  | x :: _ when Base.Char.is_digit x ->
      (* trailing dot is allowed for now *)
      let digits, r =
        Base.List.split_while rest ~f:(fun c ->
            Base.Char.is_digit c || c == '.')
      in
      let f = digits |> Base.String.of_char_list |> Float.of_string in
      lex_r (LexNumber f :: acc) r
  | x :: _ when Base.Char.is_alpha x -> (
      let identifier, r =
        Base.List.split_while rest ~f:Base.Char.is_alphanum
      in
      let s = Base.String.of_char_list identifier in
      match Base.Hashtbl.find keywords s with
      | Some k ->
          lex_r (k :: acc) r
      | _ ->
          lex_r (LexIdentifier identifier :: acc) r )
  | x :: irest ->
      lex_r (Unknown [x] :: acc) irest

let lex_token_to_s c =
  match c with
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
  | CurlyBraceLeft ->
      "{"
  | CurlyBraceRight ->
      "}"
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
  | LexString s ->
      "\"" ^ Base.String.of_char_list s ^ "\""
  | LexNumber n ->
      Float.to_string n
  | LexIdentifier s ->
      Base.String.of_char_list s
  | Unknown c ->
      "(Unknown " ^ Base.String.of_char_list c ^ ")"

let lex s = lex_r [] (Base.String.to_list s) |> List.rev

let%test _ =
  lex "and or class fun true false class for nil"
  = [And; Or; Class; Fun; True; False; Class; For; Nil]

let%test _ =
  lex "and 123.4 or (){},."
  = [ And
    ; LexNumber 123.4
    ; Or
    ; ParenLeft
    ; ParenRight
    ; CurlyBraceLeft
    ; CurlyBraceRight
    ; Comma
    ; Dot ]

let%test _ =
  lex "-+;*/@!.!="
  = [Minus; Plus; SemiColon; Star; Slash; Unknown ['@']; Bang; Dot; BangEqual]

let%test _ =
  lex ".!====<=<>>=// foo"
  = [Dot; BangEqual; EqualEqual; Equal; LessEqual; Less; Greater; GreaterEqual]

let%test _ = lex " abc\n" = [LexIdentifier ['a'; 'b'; 'c']]

let%test _ = lex "!\"hey\"!" = [Bang; LexString ['h'; 'e'; 'y']; Bang]

let%test _ = lex "\"a!" = [Unknown ['"'; 'a'; '!']]
