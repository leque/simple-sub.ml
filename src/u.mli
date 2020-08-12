module Ref : sig
  type 'a t = 'a ref = { mutable contents : 'a; }
  val replace : 'a ref -> f:('a -> 'a) -> unit
end

val opt_merge : f:('a -> 'a -> 'a) -> 'a option -> 'a option -> 'a option
val opt_map2 : f:('a -> 'b -> 'c) -> 'a option -> 'b option -> 'c option
