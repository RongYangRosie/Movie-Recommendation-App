(* A similar pair module can be used to calculate the similarity score *)

type item 
type t = item * item 

val get_cosine_similarity: t -> float 
val get_pearson_similarity: t -> float 
val get_average_similarity: t -> float 