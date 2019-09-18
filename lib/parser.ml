type literal_value = false | true | EFloat of float | Nil | EString of string

type expr =
  | Binary of expr * Lex.lex_token * expr
  | Grouping of expr
  | Literal of literal_value
  | Unary of Lex.lex_token * expr
  | Error

let primary tokens =
  match tokens with
  | [] ->
      (Error, [])
  | Lex.False :: rest ->
      (Literal false, rest)
  | Lex.True :: rest ->
      (Literal true, rest)
  | Lex.Nil :: rest ->
      (Literal Nil, rest)
  | Lex.LexNumber f :: rest ->
      (Literal (EFloat f), rest)
  | Lex.LexString s :: rest ->
      (Literal (EString (Base.String.of_char_list s)), rest)
  | _ :: rest ->
      (Literal Nil, rest)

let rec unary tokens =
  match tokens with
  | Lex.Bang :: rest ->
      let right, rrest = unary rest in
      (Unary (Lex.Bang, right), rrest)
  | _ ->
      primary tokens

(* let multiplication e tokens = unary e tokens *)
(* let addition e tokens = multiplication e tokens *)
(* let comparison e tokens = addition e tokens *)
(* let rec equality e tokens = match tokens with *)
(* | Lex.BangEqual :: trest -> *)
(*     equality (Binary e Lex.BangEqual equality ) trest *)
(*         (1* let right = comparison enew trest in Binary(e Lex.BangEqual right) *1) *)
(* | _ -> comparison e tokens *)

(* let expression () = equality *)
(* let grouping _ = Grouping (Literal Nil) *)

(* let parse_r acc rest = *)
(*   match rest with *)
(*   | [] -> *)
(*       acc *)
(*   | lex.false :: _ -> *)
(*       literal false *)
(*   | lex.true :: _ -> *)
(*       literal true *)
(*   | lex.nil :: _ -> *)
(*       literal nil *)
(*   | lex.lexnumber f :: _ -> *)
(*       literal (efloat f) *)
(*   | lex.lexstring s :: _ -> *)
(*       literal (estring (base.string.of_char_list s)) *)
(*   | Lex.LeftParen :: irest -> *)
(*       grouping irest *)
(*   | _ -> *)
(*       Literal Nil *)

let rec expr_to_s e =
  match e with
  | Binary (l, t, r) ->
      "(Binary " ^ expr_to_s l ^ " " ^ Lex.lex_token_to_s t ^ " " ^ expr_to_s r
      ^ ")"
  | Grouping m ->
      "(Grouping " ^ expr_to_s m ^ ")"
  | Literal v ->
      let literal_value_s =
        match v with
        | false ->
            "false"
        | true ->
            "true"
        | EFloat f ->
            Float.to_string f
        | EString s ->
            s
        | Nil ->
            "nil"
      in
      "(Literal " ^ literal_value_s ^ ")"
  | Unary (t, r) ->
      "(Unary " ^ Lex.lex_token_to_s t ^ " " ^ expr_to_s r ^ ")"
  | Error ->
      "(Error)"

let%test _ = primary [Lex.False] = (Literal false, [])

let%test _ = primary [Lex.True] = (Literal true, [])

let%test _ = primary [Lex.Nil] = (Literal Nil, [])

let%test _ = primary [Lex.LexNumber 3.] = (Literal (EFloat 3.), [])

let%test _ = primary [Lex.LexString ['a'; 'b']] = (Literal (EString "ab"), [])

let%test _ =
  unary [Lex.Bang; Lex.LexNumber 1.]
  = (Unary (Lex.Bang, Literal (EFloat 1.)), [])
