type movie = {
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
}
val pp_movie : Format.formatter -> movie -> unit
val show_movie : movie -> string

type t = movie list
val pp : Format.formatter -> t -> unit
val show : t -> string

type credit = {
  movie_id : string;
  title : string;
  cast : string;
  crew : string;
}
val pp_credit : Format.formatter -> credit -> unit
val show_credit : credit -> string

type credits = credit list
val pp_credits : Format.formatter -> credits -> unit
val show_credits : credits -> string

type basic_movie = {
  genres : string;
  id : string;
  keywords : string;
  overview : string;
  popularity : string;
  title : string;
  vote_average : string;
  vote_count : string;
}
val pp_basic_movie : Format.formatter -> basic_movie -> unit
val show_basic_movie : basic_movie -> string

type movies = basic_movie list
val pp_movies : Format.formatter -> movies -> unit
val show_movies : movies -> string

val find_idx_by_movieid : movie_list:(t) -> int -> int
val find_movieid_by_idx : movie_list:(t) -> int -> int 
val find_movieid_by_title : movie_list:(t) -> string -> int 
val find_title_by_idx : movie_list:(t) -> int -> string