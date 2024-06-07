open Option_monad

module type COMPARABLE = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val empty : unit -> t
  val is_empty : t -> bool
  val size : t -> int
  val add : elt -> t -> unit
  val read_min : t -> elt option
  val remove_min : t -> elt option
end

module Make (C : COMPARABLE) : S with type elt = C.t = struct
  type elt = C.t
  type t = elt Vector.t

  let empty : unit -> t = Vector.empty
  let is_empty : t -> bool = Vector.is_empty
  let size : t -> int = Vector.size

  let rec move_up (q : t) (index : int) : unit =
      ignore
      @@
      let parent_index = (index - 1) / 2 in
      let* value = Vector.get q index in
      let* parent_value = Vector.get q parent_index in
      if C.compare parent_value value < 0
      then return ()
      else
        let* _ = Vector.set q index parent_value in
        let* _ = Vector.set q parent_index value in
        return @@ move_up q parent_index


  let add (e : elt) (q : t) : unit =
      let index = Vector.push q e in
      move_up q index


  let read_min : t -> elt option = Fun.flip Vector.get 0

  let rec move_down (q : t) (index : int) : unit =
      ignore
      @@
      let left_child_index = (2 * index) + 1 in
      let right_child_index = left_child_index + 1 in
      let* value = Vector.get q index in
      let* left_child_value = Vector.get q left_child_index in
      let* new_value, new_index =
          match Vector.get q right_child_index with
          | None -> return (left_child_value, left_child_index)
          | Some right_child_value ->
              if C.compare right_child_value left_child_value < 0
              then return (right_child_value, right_child_index)
              else return (left_child_value, left_child_index)
      in
      if C.compare new_value value < 0
      then
        let* _ = Vector.set q new_index value in
        let* _ = Vector.set q index new_value in
        return @@ move_down q new_index
      else return ()


  let remove_min (q : t) : elt option =
      let last_index = Vector.size q - 1 in
      let* last_value = Vector.get q last_index in
      let* min = Vector.set q 0 last_value in
      move_down q 0; return min
end
