(* open Data_preprocessing *)
open Demographic_mode
(* open Content_based_mode *)

let _ =
  let movies = Data_preprocessing.load_movie_data "./dataset/tmdb_5000_credits.csv" "./dataset/tmdb_5000_movies.csv" in
  match Sys.argv.(1) with
  | "rating" ->
    Data_preprocessing.parse_rating "ratings_small.csv" |>
    Rating.show |> print_endline
  | "movie" -> 
    Movie.show movies |> print_endline
  | "average" -> 
      vote_average movies |> Printf.printf "%f\n"                            (* 1 *)
  | "quantile" ->               
      quantile movies |> Printf.printf "%f\n"                                (* 2 *)
  | "vote_count" ->               
      filter_vote_count movies |> Movie.show |> print_endline                (* 3 *)
  | "sort_by_weighted_rating" ->                 
      sort_by_weighted_rating movies |> Movie.show |> print_endline          (* 4 *)
  | "sort_by_popularity" ->       
      sort_by_popularity movies |> Movie.show |> print_endline               (* 5 *)
  | "sort" -> sort movies |> Movie.show |> print_endline                     (* 6 *)
  | "n" -> 
      Demographic_mode.get_recommendations movies (int_of_string Sys.argv.(2)) |> 
      Movie.show |> print_endline
  | _ -> failwith "invalid input"

