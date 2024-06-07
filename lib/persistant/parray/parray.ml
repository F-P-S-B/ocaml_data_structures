type 'a t = 'a data ref

and 'a data =
    | Arr of 'a array
    | Diff of int * 'a * 'a t

let make n v = ref (Arr (Array.make n v))

let reroot t =
    let rec aux t =
        match !t with
        | Arr a -> a
        | Diff (i, v, next) ->
            let a = aux next in
            let old_v = a.(i) in
            a.(i) <- v;
            t := Arr a;
            next := Diff (i, old_v, t);
            a
    in
    ignore @@ aux t;
    t


let rec get a i =
    match !a with
    | Diff (ind, v, _) when ind = i ->
        ignore @@ reroot a;
        v
    | Diff (_, _, a) ->
        ignore @@ reroot a;
        get a i
    | Arr a -> a.(i)


let set a i v =
    match !a with
    | Arr arr ->
        let value = arr.(i) in
        arr.(i) <- v;
        let t = ref (Arr arr) in
        a := Diff (i, value, t);
        t
    | Diff _ -> reroot @@ ref (Diff (i, v, a))
