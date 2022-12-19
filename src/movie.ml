open Core

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

type credit =
  {
    movie_id: int;
    title: string;
    cast: string;
    crew: string;
  } [@@deriving show]

type credits = credit list [@@deriving show]

type basic_movie =
  {
    genres: string;
    homepage: string;
    id: int;
    keywords: string;
    overview: string;
    popularity: float;
    release_date: string;
    title: string;
    vote_average: float;
    vote_count: int;
  } [@@deriving show]

type movies = movie list [@@deriving show]

let find_idx_by_movieid ~(movie_list: t) (req_id: int): int =
  let idx, _ = List.findi_exn ~f:(fun _ elt -> elt.movie_id = req_id) movie_list in idx

let find_movieid_by_idx ~(movie_list: t) (req_idx: int): int = 
  let movie = List.nth_exn movie_list req_idx in movie.movie_id

let find_movie_by_idx ~(movie_list: t) (req_idx: int): movie = 
  List.nth_exn movie_list req_idx

let find_movie_by_movieid ~(movie_list: t) (req_id: int): movie = 
  List.find_exn ~f:(fun elt -> elt.movie_id = req_id) movie_list

let find_movieid_by_title ~(movie_list: t) (req_title: string): int = 
  let movie = List.find_exn ~f:(fun elt -> String.(elt.title = req_title)) movie_list in movie.movie_id

let find_title_by_movieid ~(movie_list: t) (req_id: int) : string = 
  let movie = find_movie_by_movieid ~movie_list req_id in movie.title