type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val enqueue : 'a -> 'a t -> 'a t
val dequeue : 'a t -> ('a * 'a t) option
val peek : 'a t -> 'a option
