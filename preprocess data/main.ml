let _ = 
  Printexc.record_backtrace true;
  if Sys.argv.(1) = "rating" then
    Rating.csv2list "ratings_small.csv" |>
    Rating.show |> 
    print_endline
  else
    Movie.csv2list "tmdb_5000_movies.csv" "tmdb_5000_credits.csv" |>
    Movie.show |> print_endline