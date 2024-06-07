open Option_monad

exception Invariant_Not_Respected of string

type 'a t = {
    mutable array : 'a option Array.t;
    mutable size : int;
  }

let extract_opt (value : 'a option) (func_name : string) : 'a =
    match value with
    | Some v -> v
    | None -> raise @@ Invariant_Not_Respected func_name


let equals (v1 : 'a t) (v2 : 'a t) : bool =
    let exception Neq in
    try
      if v1.size <> v2.size
      then false
      else begin
        for i = 0 to v1.size - 1 do
          if v1.array.(i) <> v2.array.(i) then raise Neq
        done;
        true
      end
    with Neq -> false


let empty () : 'a t = { array = Array.make 16 None; size = 0 }
let size (v : 'a t) : int = v.size
let is_empty (v : 'a t) : bool = v.size = 0

let resize (v : 'a t) : unit =
    let new_size =
        if v.size = Array.length v.array
        then Some (2 * v.size)
        else if v.size < int_of_float @@ log @@ float @@ Array.length v.array
        then Some v.size
        else None
    in
    match new_size with
    | None -> ()
    | Some size ->
        let new_arr = Array.make size None in
        for i = 0 to size - 1 do
          new_arr.(i) <- v.array.(i)
        done;
        v.array <- new_arr


let push (v : 'a t) (element : 'a) : int =
    resize v;
    v.array.(v.size) <- Some element;
    v.size <- v.size + 1;
    v.size - 1


let pop (v : 'a t) : 'a option =
    resize v;
    if v.size = 0
    then None
    else begin
      v.size <- v.size - 1;
      Some (extract_opt v.array.(v.size + 1) "pop")
    end


let get (v : 'a t) (i : int) : 'a option =
    if i < 0 || i >= v.size then None else Some (extract_opt v.array.(i) "get")


let set (v : 'a t) (i : int) (new_val : 'a) : 'a option =
    if i < 0 || i >= v.size
    then None
    else
      let old_val = Some (extract_opt v.array.(i) "set") in
      v.array.(i) <- Some new_val;
      old_val


let from_array (arr : 'a array) : 'a t =
    { array = Array.map (fun e -> Some e) arr; size = Array.length arr }


let from_array_inplace (arr : 'a option array) : 'a t =
    { array = arr; size = Array.length arr }


let to_list (v : 'a t) : 'a list =
    let res = ref [] in
    for i = v.size - 1 downto 0 do
      let e = extract_opt v.array.(i) "to_list" in
      res := e :: !res
    done;
    v.array |> Array.to_list |> List.filter_map Fun.id


let of_list (l : 'a list) : 'a t =
    l |> List.map (fun e -> Some e) |> Array.of_list |> from_array_inplace


let map (f : 'a -> 'b) (v : 'a t) : 'b t =
    let new_arr = Array.make (Array.length v.array) None in
    for i = 0 to v.size - 1 do
      let e = extract_opt v.array.(i) "map" in
      new_arr.(i) <- return @@ f e
    done;
    { array = new_arr; size = v.size }


let map_inplace (f : 'a -> 'a) (v : 'a t) : unit =
    for i = 0 to v.size - 1 do
      v.array.(i) <- return @@ f @@ extract_opt v.array.(i) "map_inplace"
    done


let mapi (f : int -> 'a -> 'b) (v : 'a t) : 'b t =
    let new_arr = Array.make (Array.length v.array) None in
    for i = 0 to v.size - 1 do
      let e = extract_opt v.array.(i) "map" in
      new_arr.(i) <- return @@ f i e
    done;
    { array = new_arr; size = v.size }


let mapi_inplace (f : int -> 'a -> 'a) (v : 'a t) : unit =
    for i = 0 to v.size - 1 do
      let e = extract_opt v.array.(i) "map_inplace" in
      v.array.(i) <- return @@ f i e
    done


let fold_left (f : 'acc -> 'a -> 'acc) (default : 'acc) (v : 'a t) : 'acc =
    let acc = ref default in
    for i = 0 to v.size - 1 do
      let e = extract_opt v.array.(i) "fold_left" in
      acc := f !acc e
    done;
    !acc
