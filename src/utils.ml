open Core
open Movie
open Rating

(* Change the string to lowercase, and remove the whitespace *)
let clean_string (s:string) : string = 
  s |> String.lowercase |> String.split ~on:' ' |> String.concat

let combine_helper (str_list: string list) : string = 
  str_list |> List.map ~f:clean_string |> Fn.flip List.take 3 |> String.concat ~sep:" "

let combine_field (m: movie) : string = 
  [
    combine_helper m.keywords;
    combine_helper m.cast;
    clean_string m.director;
    combine_helper m.genres;
  ] |> String.concat ~sep:" "

let shuffle (lst: 'a list) : 'a list =
  List.map ~f:(fun elt -> (Random.bits (), elt)) lst
  |> List.sort ~compare:(fun (r1, _) -> fun (r2, _) -> (Int.compare r1 r2))
  |> List.map ~f:(fun (_, elt) -> elt)

let get_max_userid (rating_list: Rating.t): int =
  rating_list 
  |> List.map ~f:(fun elt -> elt.userid)
  |> List.sort ~compare:(fun a -> fun b -> Int.descending a b)
  |> List.hd_exn

let get_random_movies ~(n:int) ~(movie_list: Movie.t) : string list =
  List.map movie_list ~f:(fun elt -> elt.title) 
  |> shuffle
  |> Fn.flip List.take n
  
let flatten_string_list (slst: string list) : string =
  List.take slst 3 |> Core.String.concat ~sep:", "

let wrap_user_ratings (lst: (string * float) list) ~(userid: int) ~(movie_list: Movie.t): Rating.t = 
  List.map lst ~f:(fun (t, r) -> 
    {
      userid = userid;
      movieid = find_movieid_by_title ~movie_list t;
      rating = r;
    }
  )