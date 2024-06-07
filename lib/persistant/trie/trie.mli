type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val find : string -> 'a t -> 'a option
val add : string -> 'a -> 'a t -> 'a t
val of_list : (string * 'a) list -> 'a t -> 'a t
