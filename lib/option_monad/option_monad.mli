val return : 'a -> 'a option
val fail : 'a option
val bind : 'a option -> ('a -> 'b option) -> 'b option
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val has_failed : 'a option -> bool
val run : 'a option -> 'a
