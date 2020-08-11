type t =
  | Int of int
  | Var of string
  | Lam of string * t
  | App of t * t
  | Record of (string * t) list
  | Selection of t * string
  | LetIn of bool * string * t * t

val pp : Format.formatter -> t -> unit
val show : t -> string

module O : sig
  val i : int -> t
  val v : string -> t
  val lamb : string -> t -> t
  val ( $ ) : t -> t -> t
  val r : (string * t) list -> t
  val ( .*() ) : t -> string -> t
  val let_in : string -> t -> t -> t
  val let_rec_in : string -> t -> t -> t
end
