open Option_monad

type 'a t = {
    front : 'a list;
    rear : 'a list;
  }

let empty = { front = []; rear = [] }
let is_empty ({ front; rear } : 'a t) : bool = front = [] && rear = []

let push_back (e : 'a) ({ front; rear } : 'a t) : 'a t =
    { front; rear = e :: rear }


let push_front (e : 'a) ({ front; rear } : 'a t) : 'a t =
    { front = e :: front; rear }


let split (l : 'a list) : 'a list * 'a list =
    let rec aux l1 l2 acc =
        match l1, l2 with
        | h :: t, [] | h :: t, _ :: [] -> h :: acc, t
        | h :: t1, _ :: _ :: t2 -> aux t1 t2 (h :: acc)
        | _, _ -> failwith "Impossible"
    in
    let l1, l2 = aux l l [] in
    List.rev l1, l2


let rec pop_front ({ front; rear } : 'a t) : ('a * 'a t) option =
    match front, rear with
    | [], [] -> fail
    | h :: front, rear -> return @@ (h, { front; rear })
    | [], l ->
        let l1, l2 = split l in
        pop_front { front = List.rev l2; rear = l1 }


let pop_back ({ front; rear } : 'a t) : ('a * 'a t) option =
    let* e, { front; rear } = pop_front { front = rear; rear = front } in
    return (e, { front = rear; rear = front })


let peek_front (d : 'a t) : 'a option =
    let* e, _ = pop_front d in
    return e


let peek_back (d : 'a t) : 'a option =
    let* e, _ = pop_back d in
    return e
