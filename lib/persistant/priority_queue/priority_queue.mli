module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val empty : t
  val merge : t -> t -> t
  val insert : elt -> t -> t
  val get_min : t -> elt option
  val extract_min : t -> (elt * t) option
end

module Make (C : COMPARABLE) : S with type elt = C.t
