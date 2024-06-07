module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module Make (C : COMPARABLE) = struct
  type elt = C.t
  type t = elt Vector.t

  let empty : unit -> t = Vector.empty
  let is_empty : t -> bool = Vector.is_empty
end
