type movie = {
  movie_id: int;
  title: string; 
  cast: string list;
  director: string;
  keywords: string list;
  genres: string list;
  overview: string;   
  popularity: float; 
  vote_count: int; 
  vote_average: float; 
} [@@deriving show]

type t = movie list [@@deriving show]

let csv2list movies credits =
  let movies = Csv.load movies in
  let credits = Csv.load credits in
  Csv.print_readable movies ;
  Csv.print_readable credits ;