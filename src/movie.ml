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
  vote_count: float;
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
    budget: string;
    genres: string;
    homepage: string;
    id: int;
    (*keywords: string;*)
    original_language: string;
    original_title: string;
    overview: string;
    popularity: float;
    production_companies: string;
    production_countries: string;
    release_date: string;
    revenue: string;
    runtime: string;
    spoken_languages: string;
    status: string;
    tagline: string;
    title: string;
    vote_average: float;
    vote_count: float;
  } [@@deriving show]

type movies = basic_movie list [@@deriving show]

type keyword_file =
{
   id: int;
   keyword: string; 
} [@@deriving show]

type keywords_movie = keyword_file list [@@deriving show]

let find_idx_by_movieid ~(movie_list: t) (req_id: int): int =
  let idx, _ = List.findi_exn ~f:(fun _ elt -> elt.movie_id = req_id) movie_list in idx

let find_movieid_by_idx ~(movie_list: t) (req_idx: int): int = 
  let movie = List.nth_exn movie_list req_idx in movie.movie_id

let find_movieid_by_title ~(movie_list: t) (req_title: string): int = 
  let movie = List.find_exn ~f:(fun elt -> String.(elt.title = req_title)) movie_list in movie.movie_id

let find_title_by_idx ~(movie_list: t) (req_idx: int): string = 
  let movie_id = find_movieid_by_idx ~movie_list req_idx in 
  let movie = List.find_exn ~f:(fun elt -> elt.movie_id = movie_id) movie_list in movie.title
