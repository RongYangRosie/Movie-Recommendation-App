(* open Movie *)

(*type movie_wr_rating = {
  movie_id : int;
  title : string;
  cast : string list;
  director : string;
  keywords : string list;
  genres : string list;
  overview : string;
  popularity : float;
  vote_count : int;
  vote_average : float;
  wr_score: float
}
*)
(*calculate mean value(C) of the all movies' vote_average, C is the mean vote across the whole report*)
(* let vote_average_mean movie_list =
  List.map (fun {vote_average ; _ } -> float_of_string vote_average) movie_list |>
  List.fold_left ( +. ) 0. |>
  (fun sum -> sum /. (List.length movie_list |> float_of_int)) *)
 
(*calculate quantile value(m), m is the minimum votes required to be listed in the chartm is the minimum votes required to be listed in the chart*)
(* let quantile movie_list =
  let vote_counts = List.map (fun { vote_count; _ } -> float_of_string vote_count) movie_list in
  Owl_stats.quantile vote_counts 0.9 *)
 
(*filter all movies whose vote_count is greater than 0.9 quantile*)
(* let filter_m movie_list =
  let m = quantile results in
  List.filter
  (fun { vote_count; _ } -> float_of_string vote_count >= m)
  movie_list *)

(*

val calculate_WR movie_list: movie list ->movie_wr_rating list

(*
  calculate Weighted Rating = (v/(v+m) *R) + (m/(v+m) *C) of all movies   
  @param: movie list, a list of movie record
  @ret: movie list - movie_wr_rating list

*)

val popularity_sort: movie list -> movie list
(*
  sort all movies by popularity 
  @param: movie list, a list of movie record
  @ret: movie list - a list of movie record after sorting
*)



val wr_sort: movie list -> movie list
(*
  sort all movies by WR
  @param: movie list, a list of movie record
  @ret: movie list - a list of movie record after sorting
*)

val wr_popularity_sort: movie list -> movie list ->movie list
(*
  sort all movies by WR, popularity
  @param: movie list, a list of movie record
  @ret: movie list - a list of movie record after sorting
*)

*)



