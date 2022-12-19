open Core
open Sklearn.Feature_extraction.Text
open Np
open Movie
open Utils

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