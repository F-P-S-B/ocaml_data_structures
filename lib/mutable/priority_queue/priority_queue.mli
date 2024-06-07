module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val empty : unit -> t
  val is_empty : t -> bool
  val add : t -> unit
  val size : t -> int
  val read_min : t -> elt option
  val remove_min : t -> elt option
end

module Make (C : COMPARABLE) : S with type elt = C.t
