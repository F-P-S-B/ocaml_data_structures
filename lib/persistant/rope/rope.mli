type t

val height : t -> int
val length : t -> int
val concat : t -> t -> t
val to_string : t -> string
val get : t -> int -> char option
val sub : t -> int -> int -> t option
val insert : t -> string -> int -> t option
