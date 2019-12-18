let ( >> ) f g x = g (f x)

let ( <|> ) a b = match a with Some _ as s -> s | None -> b
