let efloat_plus a b =
  match (a, b) with
  | Parse.EFloat x, Parse.EFloat y ->
      Parse.EFloat (x +. y)
  | _ ->
      Parse.Nil

let rec eval exp =
  match exp with
  | Parse.Grouping e ->
      eval e
  | Parse.Unary (t, e) -> (
      let v = eval e in
      match (v, t) with
      | Parse.EFloat f, Lex.Minus ->
          Parse.EFloat (-.f)
      | _ ->
          v )
  | Parse.Literal true ->
      true
  | Parse.Literal false ->
      false
  | Parse.Literal (Parse.EFloat f) ->
      Parse.EFloat f
  | Binary (left, Lex.Plus, right) ->
      efloat_plus (eval left) (eval right)
  | _ ->
      Nil
