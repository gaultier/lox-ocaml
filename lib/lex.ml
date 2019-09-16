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
  | x :: _ when Base.Char.is_digit x ->
      (* trailing dot is allowed for now *)
      let digits, r =
        Base.List.split_while rest ~f:(fun c ->
            Base.Char.is_digit c || c == '.')
      in
      let s = Base.String.of_char_list digits in
      let f = Float.of_string s in
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
  | And ->
      "And"
  | Class ->
      "Class"
  | Else ->
      "Else"
  | False ->
      "False"
  | For ->
      "For"
  | Fun ->
      "Fun"
  | If ->
      "If"
  | Nil ->
      "Nil"
  | Or ->
      "Or"
  | Print ->
      "Print"
  | Return ->
      "Return"
  | Super ->
      "Super"
  | This ->
      "This"
  | True ->
      "True"
  | Var ->
      "Var"
  | While ->
      "While"
  | LexString s ->
      "LexString=`" ^ Base.String.of_char_list s ^ "`"
  | LexNumber n ->
      "LexNumber=" ^ Float.to_string n
  | LexIdentifier s ->
      "LexIdentifier=`" ^ Base.String.of_char_list s ^ "`"
  | Unknown c ->
      "Unknown=`" ^ Base.String.of_char_list c ^ "`"

let lex s = lex_r [] (Base.String.to_list s)

let rec fact n = if n = 1 then 1 else n * fact (n - 1);;

print_endline (string_of_int (fact 5));;

let%test _ = fact 5 = 0

