open Option_monad

type t = {
    link : int Vector.t;
    rank : int Vector.t;
  }

let create (n : int) : t =
    {
      link = Array.init n (fun i -> Some i) |> Vector.of_array_inplace;
      rank = Array.make n (Some 0) |> Vector.of_array_inplace;
    }


let add (uf : t) : int =
    let new_index = Vector.size uf.rank in
    ignore @@ Vector.push uf.link new_index;
    ignore @@ Vector.push uf.rank 0;
    new_index


let rec find (uf : t) (i : int) : int option =
    let* p = Vector.get uf.link i in
    if p = i
    then return i
    else begin
      let* r = find uf p in
      ignore @@ Vector.set uf.link i r;
      return r
    end


let union (uf : t) (i : int) (j : int) : unit option =
    let* repr_i = find uf i in
    let* repr_j = find uf j in
    if repr_i = repr_j
    then return ()
    else
      let* rank_i = Vector.get uf.rank repr_i in
      let* rank_j = Vector.get uf.rank repr_j in
      if rank_i < rank_j
      then begin
        ignore @@ Vector.set uf.link repr_i repr_j;
        return ()
      end
      else begin
        ignore @@ Vector.set uf.link repr_j repr_i;
        if rank_i = rank_j then ignore @@ Vector.set uf.rank repr_i (rank_i + 1);
        return ()
      end
