type result = {
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
val pp_result : Format.formatter -> result -> unit
val show_result : result -> string
type t = result list
val pp : Format.formatter -> t -> unit
val show : t -> string

type credit = {
  movie_id : string;
  title : string;
  cast : string;
  crew : string;
}
type credits = credit list

val pp_credit : Format.formatter -> credit -> unit
val show_credit : credit -> string
val pp_credits : Format.formatter -> credits -> unit
val show_credits : credits -> string

type movie = {
  genres : string;
  id : string;
  keywords : string;
  overview : string;
  popularity : string;
  title : string;
  vote_average : string;
  vote_count : string;
}

type movies = movie list
val parse_credit : string -> credit list
val parse_movies : string -> movie list
val csv2list : string -> string -> result list

val pp_movies : Format.formatter -> movies -> unit
val show_movies : movies -> string


val pp_movie : Format.formatter -> movie -> unit
val show_movie : movie -> string