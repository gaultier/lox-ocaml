type lex_token =
  | CurlyBraceLeft
  | CurlyBraceRight
  | LeftParen
  | RightParen
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
  | Unknown of char

let rec lex_r acc irest =
  match irest with
  | [] ->
      acc
  | '{' :: irest ->
      lex_r (CurlyBraceLeft :: acc) irest
  | '}' :: irest ->
      lex_r (CurlyBraceRight :: acc) irest
  | '(' :: irest ->
      lex_r (LeftParen :: acc) irest
  | ')' :: irest ->
      lex_r (RightParen :: acc) irest
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
  | x :: irest ->
      lex_r (Unknown x :: acc) irest

let string_to_list s = List.init (String.length s) (String.get s)

let lex_token_to_s c =
  match c with
  | LeftParen ->
      "LeftParen"
  | RightParen ->
      "RightParen"
  | Comma ->
      "Comma"
  | Dot ->
      "Dot"
  | Minus ->
      "Minus"
  | Plus ->
      "Plus"
  | SemiColon ->
      "SemiColon"
  | Star ->
      "Star"
  | Slash ->
      "Slash"
  | CurlyBraceLeft ->
      "CurlyBraceLeft"
  | CurlyBraceRight ->
      "CurlyBraceRight"
  | Bang ->
      "Bang"
  | BangEqual ->
      "BangEqual"
  | Equal ->
      "Equal"
  | EqualEqual ->
      "EqualEqual"
  | Unknown c ->
      "Unknown=`" ^ String.make 1 c ^ "`"

let lex s = lex_r [] (string_to_list s)
