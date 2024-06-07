open Option_monad

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

module Make (C : COMPARABLE) : S with type elt = C.t = struct
  type elt = C.t

  type t =
      | Leaf
      | Node of t * elt * t

  let empty = Leaf

  let rec merge t1 t2 =
      match t1, t2 with
      | Leaf, t | t, Leaf -> t
      | Node (l1, e1, r1), Node (l2, e2, r2) ->
          if C.compare e1 e2 < 0
          then Node (merge r1 t2, e1, l1)
          else Node (merge r2 t1, e2, l2)


  let insert x t = merge (Node (Leaf, x, Leaf)) t

  let get_min = function
      | Leaf -> fail
      | Node (_, e, _) -> return e


  let extract_min = function
      | Leaf -> fail
      | Node (left, e, right) -> return (e, merge left right)
end
