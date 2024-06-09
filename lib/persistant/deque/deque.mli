type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val push_back : 'a -> 'a t -> 'a t
val push_front : 'a -> 'a t -> 'a t
val pop_front : 'a t -> ('a * 'a t) option
val pop_back : 'a t -> ('a * 'a t) option
val peek_front : 'a t -> 'a option
val peek_back : 'a t -> 'a option
