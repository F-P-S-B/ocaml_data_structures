open Option_monad

type 'a node = {
    value : 'a;
    mutable next : 'a node option;
    mutable pred : 'a node option;
  }

type 'a t = {
    mutable first : 'a node option;
    mutable last : 'a node option;
  }

let empty () : 'a t = { first = None; last = None }
let is_empty ({ first; last } : 'a t) : bool = first = None && last = None

let push_back (e : 'a) (deq : 'a t) : unit =
    let new_node = { value = e; next = deq.first; pred = None } in
    if is_empty deq
    then begin
      deq.first <- Some new_node;
      deq.last <- Some new_node
    end
    else
      let first = Option.get deq.first in
      first.pred <- Some new_node;
      deq.first <- Some new_node


let push_front (e : 'a) (deq : 'a t) : unit =
    let new_node = { value = e; next = None; pred = deq.last } in
    if is_empty deq
    then begin
      deq.first <- Some new_node;
      deq.last <- Some new_node
    end
    else
      let last = Option.get deq.last in
      last.next <- Some new_node;
      deq.last <- Some new_node


let rec pop_front (deq : 'a t) : 'a option =
    if is_empty deq
    then fail
    else begin
      let first = Option.get deq.first in
      deq.first <- first.next;
      if deq.first = None then deq.last <- None;
      return first.value
    end


let pop_back (deq : 'a t) : 'a option =
    if is_empty deq
    then fail
    else begin
      let last = Option.get deq.last in
      deq.last <- last.pred;
      if deq.last = None then deq.first <- None;
      return last.value
    end


let peek_front (deq : 'a t) : 'a option =
    let* first = deq.first in
    return first.value


let peek_back (deq : 'a t) : 'a option =
    let* last = deq.last in
    return last.value
