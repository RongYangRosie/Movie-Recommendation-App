(* movie table *)
type movie = string list
type t = movie list

(*
   generate tfidf vector
   @param: movie list - t
   @ret: tfidf vector - float list list
*)
val generate_tfidf: t -> float list list

(*
   calculate cosine similarity
   @param: movie list - t
   @ret: a list of all movie pairs' title and cosine similarity score - (string * string * float) list  
*)
val calculate_cosine_similarity: t -> (string * string * float) list  

(*
   calculate pearson similarity
   @param: movie list - t
   @ret: a list of all movie pairs' title and pearson similarity score - (string * string * float) list  
*)
val calculate_pearson_similarity: t -> (string * string * float) list  

(*
   calculate average similarity
   @param: movie list - t
   @ret: a list of all movie pairs' title and average similarity score - (string * string * float) list  
*)
val calculate_average_similarity: t -> (string * string * float) list  

(*
   get n recommended movies
   @param: movie list - t, movie title - string, n - int
   @ret: movie title list - string list
*)
val get_recommended_movies: t -> string -> int -> string list