type 'a t

val empty : unit -> 'a t
val is_empty : 'a t -> bool
val push_back : 'a -> 'a t -> unit
val push_front : 'a -> 'a t -> unit
val pop_front : 'a t -> 'a option
val pop_back : 'a t -> 'a option
val peek_front : 'a t -> 'a option
val peek_back : 'a t -> 'a option
