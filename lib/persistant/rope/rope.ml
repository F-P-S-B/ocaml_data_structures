open Option_monad

type t =
    | Str of string
    | Node of {
        height : int;
        size : int;
        left : t;
        right : t;
      }

let height : t -> int = function
    | Str _ -> 0
    | Node { height; _ } -> height


let length : t -> int = function
    | Str s -> String.length s
    | Node { size; _ } -> size


let concat (r1 : t) (r2 : t) : t =
    Node
      {
        height = height r1 + height r2;
        size = length r1 + length r2;
        left = r1;
        right = r2;
      }


let collect : t -> string list =
    let rec aux acc = function
        | Str s -> s :: acc
        | Node { left; right; _ } ->
            let acc = aux acc right in
            aux acc left
    in
    aux []


let fib_array =
    let a = Array.make 80 0 in
    a.(1) <- 1;
    for i = 2 to 80 - 1 do
      a.(i) <- a.(i - 1) + a.(i - 2)
    done;
    a


let fib i = fib_array.(i)
let is_balanced (r : t) : bool = length r >= fib (height r + 2)

let rec merge (strs : string array) (i : int) (j : int) : t option =
    match i - j with
    | 1 -> ( try return @@ Str strs.(i) with Invalid_argument _ -> fail)
    | 2 -> (
        try return @@ concat (Str strs.(i)) (Str strs.(i + 1))
        with Invalid_argument _ -> fail)
    | range ->
        let mid = i + (range / 2) in
        let* left = merge strs i mid in
        let* right = merge strs mid j in
        return @@ concat left right


let balance (r : t) : t =
    if is_balanced r
    then r
    else
      let strs = Array.of_list @@ collect r in
      run @@ merge strs 0 (Array.length strs)


let to_string (r : t) : string = r |> collect |> String.concat ""

let rec get (r : t) (i : int) : char option =
    match r with
    | Str s -> ( try return s.[i] with Invalid_argument _ -> fail)
    | Node { size; _ } when i >= size -> fail
    | Node { left; _ } when i < length left -> get left i
    | Node { right; left; _ } -> get right (i - length left)


let rec sub (r : t) (pos : int) (len : int) : t option =
    match r with
    | Str s -> (
        try return @@ Str (String.sub s pos len)
        with Invalid_argument _ -> fail)
    | Node { size; _ } when pos + len > size -> fail
    | Node { left; _ } when pos + len <= length left -> sub left pos len
    | Node { left; right; _ } when pos >= length left ->
        sub right (pos - length left) len
    | Node { left; right; _ } ->
        let len_left = length left - pos in
        let len_right = len - len_left in
        let* new_left = sub left pos len_left in
        let* new_right = sub right 0 len_right in
        return @@ concat new_left new_right


let insert (r : t) (s : string) (pos : int) =
    let* left = sub r 0 pos in
    let* right = sub r pos (length r - pos) in
    return @@ balance @@ concat left (concat (Str s) right)
