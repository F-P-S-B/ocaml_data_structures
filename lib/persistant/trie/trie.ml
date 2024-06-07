open Option_monad
(* Type représentant un t.
   Pour ce type et la fonction find, voir:

    https://gist.github.com/Tchou/c0c305621bc909e189db90821d78eb6f
*)

type 'a t = Node of ('a option * (char * 'a t) list)

(** [empty] est le t vide. *)
let empty = Node (None, [])

(** [is_empty t] est vrai si et seulement si [t] est vide. *)
let is_empty t =
    match t with
    | Node (None, []) -> true
    | _ -> false


(** [find_trie key t] renvoie le (sous)-trie accessible
    dans [t] en suivant la clé [key]. *)
let find_trie key t =
    let rec find_node i t =
        if i = String.length key
        then return t
        else
          match t with
          | Node (_, l) -> find_list i l
    and find_list i l =
        let ci = key.[i] in
        match l with
        | [] -> fail
        | (d, t) :: ll ->
            if ci > d
            then find_list i ll
            else if ci = d
            then find_node (i + 1) t
            else fail
    in
    find_node 0 t


(** [find key t] renvoie la valeur accessible dans
    [t] en suivant la clé [key]. Lève l'exception [Not_found]
    si [key] n'est pas dans [t]. *)
let find key t =
    find_trie key t >>= function
    | Node (Some v, _) -> return v
    | Node (None, _) -> fail


let add key v t =
    let rec insert_node i t =
        match t with
        | Node (o, l) ->
            if i = String.length key
            then Node (Some v, l)
            else Node (o, insert_list i l)
    and insert_list i l =
        let ci = key.[i] in
        match l with
        | [] -> [ ci, insert_node (i + 1) empty ]
        | (d, t) :: ll ->
            if ci > d
            then (d, t) :: insert_list i ll
            else if ci = d
            then (ci, insert_node (i + 1) t) :: ll
            else
              (ci, insert_node (i + 1) empty)
              :: l (* attention, ici l pour laisser (d,t)*)
    in
    insert_node 0 t


let of_list l t = List.fold_left (fun acc (k, v) -> add k v acc) t l
