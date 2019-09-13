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
  | Unknown of char

let chars_to_token x =
  match x with
  | '{' ->
      CurlyBraceLeft
  | '}' ->
      CurlyBraceRight
  | '(' ->
      LeftParen
  | ')' ->
      RightParen
  | ',' ->
      Comma
  | '.' ->
      Dot
  | '-' ->
      Minus
  | '+' ->
      Plus
  | ';' ->
      SemiColon
  | '*' ->
      Star
  | '/' ->
      Slash
  | _ ->
      Unknown x

let lex_reducer acc x = chars_to_token x :: acc

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
  | Unknown c ->
      "Unknown=" ^ String.make 1 c

let lex s = s |> string_to_list |> List.fold_left lex_reducer []
