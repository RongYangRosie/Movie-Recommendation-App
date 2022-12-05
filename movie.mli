type movie =
  {
    budget: string;
    genres: string;
    homepage: string;
    id: string;
    keywords: string;
    original_language: string;
    original_title: string;
    overview: string;
    popularity: string;
    production_companies: string;
    production_countries: string;
    release_date: string;
    revenue: string;
    runtime: string;
    spoken_languages: string;
    status: string;
    tagline: string;
    title: string;
    vote_average: string;
    vote_count: string;
  }
type result = {
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
}
type credit =  {
  movie_id: string;
  title: string;
  cast: string;
  crew: string;
}
type credits = credit list
type movies = movie list
type t = result list

val parse_credit : string -> credits
val parse_movie : string -> movies 
val csv2list: movies  -> credits -> t


