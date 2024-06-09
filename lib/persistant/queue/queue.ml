open Option_monad

type 'a t = {
    front : 'a list;
    rear : 'a list;
  }

let empty : 'a t = { front = []; rear = [] }
let is_empty ({ front; rear } : 'a t) : bool = front = [] && rear = []

let enqueue (e : 'a) ({ front; rear } : 'a t) : 'a t =
    { front; rear = e :: rear }


let rec dequeue ({ front; rear } : 'a t) : ('a * 'a t) option =
    match front with
    | [] ->
        if rear = [] then fail else dequeue { front = List.rev rear; rear = [] }
    | h :: front -> return (h, { front; rear })


let peek (q : 'a t) : 'a option = dequeue q >>= fun e -> return @@ fst e
