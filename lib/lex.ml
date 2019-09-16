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
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | LexString of char list
  | LexNumber of float
  | Unknown of char list

let is_char_digit c =
  match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      true
  | _ ->
      false

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
          lex_r (Unknown s :: acc) r )
  | x :: irest when is_char_digit x ->
      lex_r (LexNumber 0. :: acc) irest
  | x :: irest ->
      lex_r (Unknown [x] :: acc) irest

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
  | Less ->
      "Less"
  | LessEqual ->
      "LessEqual"
  | Greater ->
      "Greater"
  | GreaterEqual ->
      "GreaterEqual"
  | LexString s ->
      "LexString=`" ^ Base.String.of_char_list s ^ "`"
  | LexNumber n ->
      "LexNumber=" ^ Float.to_string n ^ "\""
  | Unknown c ->
      "Unknown=`" ^ Base.String.of_char_list c ^ "`"

let lex s = lex_r [] (Base.String.to_list s)
