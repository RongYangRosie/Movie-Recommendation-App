open Data_preprocessing
open Content_based_mode

(* let show_data () = 
  Printexc.record_backtrace true;
  if Sys.argv.(1) = "rating" then
    Rating.csv2list "../dataset/ratings_small.csv" |>
    Rating.show |> 
    print_endline
  else
    Movie.csv2list "../dataset/tmdb_5000_movies.csv" "../dataset/tmdb_5000_credits.csv" |>
    Movie.show |> print_endline *)

(* This is only support Mode 2 now *)
let command = 
  Core.Command.basic ~summary:"Movie Recommendation App"
    (let%map_open.Core.Command mode = flag "--mode" (required string) ~doc:"recommendation mode"
      and title = flag "--title" (required string) ~doc:"movie title"
      and n = flag "-n" (optional_with_default 5 int) ~doc:"the number of the recommended movies"
    in fun () ->
      let movie_list = load_movie_data "./dataset/tmdb_5000_movies.csv" "./dataset/tmdb_5000_credits.csv" in 
      match mode with 
      | "content_based" -> get_recommendations ~movie_list:movie_list ~title:title ~n:n |> Core.String.concat ~sep:"\n" |> print_endline
      | _ -> "other" |> print_endline
    )

let () = 
  Command_unix.run command

