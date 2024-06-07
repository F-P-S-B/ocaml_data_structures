(** le type du tableau *)
type 'a t

(** [make n x] renvoie un tableau de taille [n]
   initialisé avec la valeur [x] *)
val make : int -> 'a -> 'a t

(** [get a n] renvoie la valeur à indice [n] du tableau [a] *)
val get : 'a t -> int -> 'a option

(** [set a n x] renvoie un nouveau tableau mettant à jour [a]
     avec la valeur [x] à l'indice [n] *)
val set : 'a t -> int -> 'a -> 'a t
