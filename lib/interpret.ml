let efloat_op_float a b op =
  match (a, b) with
  | Parse.EFloat x, Parse.EFloat y ->
      Parse.EFloat (op x y)
  | _ ->
      Parse.Nil

let efloat_op_bool a b op =
  match (a, b) with
  | Parse.EFloat x, Parse.EFloat y ->
      Parse.Bool (op x y)
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
  | Parse.Literal (Bool true as b) | Parse.Literal (Bool false as b) ->
      b
  | Parse.Literal (Parse.EFloat f) ->
      Parse.EFloat f
  | Parse.Binary (left, Lex.Plus, right) ->
      efloat_op_float (eval left) (eval right) ( +. )
  | Parse.Binary (left, Lex.Minus, right) ->
      efloat_op_float (eval left) (eval right) ( -. )
  | Parse.Binary (left, Lex.Slash, right) ->
      efloat_op_float (eval left) (eval right) ( /. )
  | Parse.Binary (left, Lex.Star, right) ->
      efloat_op_float (eval left) (eval right) ( *. )
  | Parse.Binary (left, Lex.Less, right) ->
      efloat_op_bool (eval left) (eval right) ( < )
  | Parse.Binary (left, Lex.LessEqual, right) ->
      efloat_op_bool (eval left) (eval right) ( <= )
  | Parse.Binary (left, Lex.Greater, right) ->
      efloat_op_bool (eval left) (eval right) ( > )
  | Parse.Binary (left, Lex.GreaterEqual, right) ->
      efloat_op_bool (eval left) (eval right) ( >= )
  | _ ->
      Nil
