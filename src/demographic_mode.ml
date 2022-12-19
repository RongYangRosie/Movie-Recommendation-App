open Core
(*
   calculate average value of vote_average
   @param: movie record list 
   @ret:  float - average of vote_average field of the entire movie record list
*)
let vote_average (l: Movie.t): float =
  let sum =
    List.map l ~f:(fun { vote_average; _ } -> vote_average) |>
    List.fold ~init:0. ~f:( +. )
  in
  let len = List.length l in
  Float.O.(sum / of_int len)

(*
   calculate quantile value of movie record list
   @param: movie record list 
   @ret: quantile
*)
let quantile (l: Movie.t): float =
  let vote_counts =
    List.map l ~f:(fun { vote_count ; _} -> float_of_int vote_count) |>
    Array.of_list
  in
  Owl_stats.quantile vote_counts  0.9

(*
   filter movie record list and get movies whose vote_count are greater than quantile
   @param: movie record list
   @ret: movie record list
*)
let filter_vote_count (l: Movie.t): Movie.t = 
  let m = quantile l in
  List.filter l ~f:(fun { vote_count; _ } -> Float.compare (float_of_int vote_count) m >= 0)

(*
  sort movie record list by weighted_rating
  weighted_rating = v / (v + m) * R + m / (v + m) * C
  - v is the number of votes for the movie [movie's vote_count]
  - m is the minimum votes required to be listed in the chart [quantile]
  - C is the mean vote across the whole report[average of vote_count]
  - R is the average rating of the movie[movie's vote_average]
  @param: movie record list
  @ret: movie record list
*)
let sort_by_weighted_rating (l: Movie.t): Movie.t =
  let c = vote_average l in
  let m = quantile l in
  let weighted_rating ({ vote_count; vote_average; _ }: Movie.movie) =
    let r = vote_average in
    let v = float_of_int vote_count in
    Float.O.(v / (v + m) * r + m / (v + m) * c)
  in
  List.sort l ~compare:(fun (x: Movie.movie) y ->
    let x = weighted_rating x in
    let y = weighted_rating y in
    Float.compare x y) |>
  List.rev

(*
   sort movie record list by movie's popularity
   @param: movie record list
   @ret: movie record list
*)
let sort_by_popularity (l: Movie.t): Movie.t =
  List.sort l ~compare:(fun x y -> Float.compare x.popularity y.popularity) |>
  List.rev

(*
   sort movie record list and follow rules:
   - get the average of the rankings ranked by weighted_rating and popularity, who is bigger will rank higher
   - if the means are the same, rank them by their vote_count
   @param: movie record list
   @ret: movie record list
*)
let sort (l: Movie.t): Movie.t =
  let lwr = sort_by_weighted_rating l |> List.mapi ~f:(fun i m -> i + 1, ((m: Movie.movie).movie_id)) in
  let lp = sort_by_popularity l |> List.mapi ~f:(fun i m -> i + 1, m) in
  let map = 
    List.fold lwr ~init:(Map.empty(module Int)) ~f:(fun acc (idx, movie_id) ->
      Map.add_exn acc ~key:movie_id ~data:idx)
  in
  let newl =
    List.map lp ~f:(fun (idx1, ({movie_id; _} as movie)) ->
      let idx2 = Map.find_exn map movie_id in
      let idx = (idx1 + idx2) / 2 in
      (idx, movie))
  in
  List.sort newl ~compare:(fun (idx1, { vote_count = count1; _ }) (idx2, { vote_count = count2; _ }) ->
    let cmp = Int.compare idx1 idx2 in
    if cmp <> 0 then cmp
    else Int.compare count1 count2) |>
  List.rev |>
  List.map ~f:snd

(*
   Enter the number n, and get n recommended movie information
   @param: movie record list,  int - Number of recommended movies
   @ret: movie record list
*)
let get_recommendations (l: Movie.t) (n: int): Movie.t = List.take (sort l) n