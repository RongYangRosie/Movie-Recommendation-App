(* movie table *)
type movie = string list
type t = movie list

(* calculate popular score for movie 
   @param: movie - movie
   @ret: score - float
*)
val calculate_score: movie -> float

(* sort movie list by popularity score 
   @param: movie list - t 
   @ret: sorted movie list - t
*)
val sort_by_popularity: t -> t

(* get n recommended movies 
   @param: movie list - t, n - int
   @ret: movie title list - string list
*)
val get_recommended_movies: t -> int -> string list