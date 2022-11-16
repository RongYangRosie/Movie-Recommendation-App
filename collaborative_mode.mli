(* movie table *)
type movie = string list
type t = movie list

(*
   calculate mean RMSE and MAE of algorithm SVD
   @param: movie list - t, fold number -> int
   @ret: RMSE and MAE - float * float
*)
val calculate_mean_RMSE_MAE: t -> int -> (float * float)
(*
   train and fit svd model
   @param: movie list - t
   @ret: a list of user_id * movie_id * rating -  (string * string * float) list
*)
val train_and_fit_svd: t -> (string * string * float) list

(* 
   get n recommended movies 
   @param: movie list - t, a list of movie title * rating - string, n - int
   @ret: movie title list - string list
*)
val get_recommended_movies: t -> (string * float) list -> int -> string list