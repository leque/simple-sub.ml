type t =
  | Int of int
  | Var of string
  | Lam of string * t
  | App of t * t
  | Record of (string * t) list
  | Selection of t * string
  | LetIn of bool * string * t * t
[@@deriving show]

module O = struct
  let i n = Int n

  let v n = Var n

  let lamb name body = Lam (name, body)

  let ($) f a = App (f, a)

  let r fs = Record fs

  let (.*()) r label =
    Selection (r, label)

  let let_in name expr body =
    LetIn (false, name, expr, body)

  let let_rec_in name expr body =
    LetIn (true, name, expr, body)
end
