open Core
open Sklearn.Feature_extraction.Text
open Np
open Movie
open Utils

(* type movie = {
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
type t = movie list *)

(* let movie_list = [
  {
    movie_id = 1;
    title = "Avatar";
    cast = ["Sam Worthington"; "Zoe Saldana"; "Sigourney Weaver"];
    director = "James Cameron";
    keywords = ["culture clash"; "future"; "space war"];
    genres =  ["Action"; "Adventure"; "Fantasy"];
    overview = "In the 22nd century, a paraplegic Marine is dispatched to the moon Pandora on a unique mission, but becomes torn between following orders and protecting an alien civilization.";
    popularity = 150.437577;
    vote_count = 11800;
    vote_average = 7.2;
  };
  {
    movie_id = 2;
    title = "Spectre";
    cast = ["Daniel Craig"; "Christoph Waltz"; "Léa Seydoux"];
    director = "Sam Mendes";
    keywords = ["spy"; "based on novel"; "secret agent"];
    genres = ["Action"; "Adventure"; "Crime"];
    overview = "A cryptic message from Bond's past sends him on a trail to uncover a sinister organization. While M battles political forces to keep the secret service alive, Bond peels back the layers of deceit to reveal the terrible truth behind SPECTRE.";
    popularity = 107.376788;
    vote_count = 4466;
    vote_average = 6.3;
  };

] *)

let combine_field (m: movie) : string = 
  [
    combine_helper m.keywords;
    combine_helper m.cast;
    clean_string m.director;
    combine_helper m.genres;
  ] |> String.concat ~sep:" "

let generate_tfidfvector (movie_list: t) = 
  let overview_arr = Numpy.Ndarray.of_string_list @@ List.map ~f:(fun m -> m.overview) movie_list in 
  let tfidf_vectorizer = TfidfVectorizer.create 
    ~input: `Content
    ~lowercase: true
    ~stop_words: `English () in 
  TfidfVectorizer.fit_transform ~raw_documents:overview_arr tfidf_vectorizer

let generate_countvector (movie_list: t) = 
  let soup_list = List.map ~f:combine_field movie_list in 
  let soup_arr = Numpy.Ndarray.of_string_list soup_list in 
  let count_vectorizer = CountVectorizer.create 
    ~input: `Content
    ~lowercase: true
    ~stop_words: `English () in 
  CountVectorizer.fit_transform ~raw_documents:soup_arr count_vectorizer

let combine_vector (vect_list: 'a list) = 
  let vect_list = Py.List.of_list @@ List.map ~f:Obj.to_pyobject vect_list 
  in Obj.of_pyobject @@ Scipy.Sparse.hstack ~format:"csr" ~blocks:vect_list ()

let type_converter (arr) : float list list = 
  Obj.to_pyobject arr |> 
  Py.Sequence.to_list_map (Py.Sequence.to_list_map (Py.Float.to_float)) 

let calculate_cosine_similarity (movie_list: t) : float list list = 
  let combined_vector = 
    [generate_tfidfvector; generate_countvector] 
    |> List.map ~f:(fun f -> f movie_list)
    |> combine_vector
  in Obj.of_pyobject @@ Sklearn.Metrics.Pairwise.cosine_similarity ~x:combined_vector () |> type_converter

let desc_sort_similarity (sim: float list list) (idx: int) : (int * float) list = 
  List.nth_exn sim idx
  |> List.mapi ~f:(fun idx elt -> (idx, elt))
  |> List.sort ~compare:(fun (_, sim_a) -> fun (_, sim_b) -> Float.descending sim_a sim_b) 
  |> List.tl_exn (* remove the first item, i.e., itself *)

let get_recommendations ~(title: string) ~(n: int) ~(movie_list: t) : Movie.t = 
  let sim = calculate_cosine_similarity movie_list in 
  let idx = find_movieid_by_title ~movie_list title |> find_idx_by_movieid ~movie_list in 
  desc_sort_similarity sim idx
  |> Fn.flip List.take n
  |> List.map ~f:(fun (i, _) -> find_movie_by_idx ~movie_list i)