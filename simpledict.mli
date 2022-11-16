(* A simple dict module *)
type key
type value 
type t

val empty : t 

val size: t -> int 

val get: key -> t -> value option 

val insert: key -> value -> t -> t

val remove: key -> t -> t * value option

(* convert assoc list into simpledict format *)
val from_assoc_list: (key * value) list -> t

(* convert simpledict into assoc list format *)
val to_assoc_list: t -> (key * value) list