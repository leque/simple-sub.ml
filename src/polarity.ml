type t = Positive | Negative
[@@deriving eq, ord, show]

let negate = function
  | Positive -> Negative
  | Negative -> Positive

let values = [Positive; Negative]
