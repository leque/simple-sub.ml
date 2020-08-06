let id = fun x -> x
let twice = fun f -> fun x -> f (f x)

let object1 = { x = 42; y = id }
let object2 = { x = 17; y = false }
let pick_an_object = fun b ->
  if b then object1 else object2

let rec produce = fun arg ->
  { head = arg; tail = produce (succ arg) }

let rec consume = fun strm ->
  add strm.head (consume strm.tail)
