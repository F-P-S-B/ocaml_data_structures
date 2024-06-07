type t

val create : int -> t
val add : t -> int
val find : t -> int -> int option
val union : t -> int -> int -> unit option
