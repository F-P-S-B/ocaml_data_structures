(** The type of vectors *)
type 'a t

(** Extensional equality: checks whether the two arrays have the same elements at the same indices *)
val equals : 'a t -> 'a t -> bool

(** Empty vector *)
val empty : unit -> 'a t

(** Adds an element at the end of the vector *)
val push : 'a t -> 'a -> int

(** Removes the last element of the vector  *)
val pop : 'a t -> 'a option

(** Takes a vector and returns wether it is empty or not *)
val is_empty : 'a t -> bool

(** Takes a vector and returns the number of elements it contains *)
val size : 'a t -> int

(** `get v n` returns the nth element of v if `0 <= n < length v` and None otherwise  *)
val get : 'a t -> int -> 'a option

(** `set v n e` sets the nth element of v to e if `0 <= n < length v` if it is in bound, and returns the element that got replaced, or None if the precondition isn't fulfilled *)
val set : 'a t -> int -> 'a -> 'a option

(** `from_array a` creates a vector with the same elements as a without consuming a  *)
val from_array : 'a array -> 'a t

(** Same as from_array except that the array passed is consumed and should not be used later. Each element must be constructed with Some (gives access to Array.make/.create for free ) *)
val from_array_inplace : 'a option array -> 'a t

(** Transforms a vector into a list  *)
val to_list : 'a t -> 'a list

(** Transforms a list into a vector  *)
val of_list : 'a list -> 'a t

(** Creates a new vector created using the mapping function on the elements of the first vector  *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Modifies the vector using the mapping function on its elements *)
val map_inplace : ('a -> 'a) -> 'a t -> unit

(** Same as map but with the indices passed in the function  *)
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

(** Same as map_inplace but with the indices passed in the function  *)
val mapi_inplace : (int -> 'a -> 'a) -> 'a t -> unit

(** fold_left *)
val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
