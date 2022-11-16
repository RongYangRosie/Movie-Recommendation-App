type item 
type pair = item * item
type value
type t = (pair * value) list

val get_pair_values: item -> t -> (pair * value) list 
val get_item_values: item -> t -> (item * value) list 
val get_sorted_pair_values: item -> reverse:(bool) -> t -> (pair * value) list 
val get_sorted_item_values: item -> reverse:(bool) -> t -> (item * value) list 
val get_n_pair_values: item -> int -> t -> (pair * value) list 
val get_n_item_values: item -> int -> t -> (item * value) list 