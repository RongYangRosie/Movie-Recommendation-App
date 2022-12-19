open Core
open Yojson
open Rating

(*
   read credit data from tmdb_5000_credits.csv, and create a credit record list
   @param: string - file name
   @ret: credit list 
*)
let parse_credit (f: string): Movie.credits =
  match Csv.load f with
  | [] -> assert false
  | header :: data ->
      Csv.associate header data |>
      List.map ~f:(fun row ->
        let lookup key = List.Assoc.find_exn row ~equal:String.equal key in
        ({ movie_id = lookup "movie_id" |> int_of_string; 
          title = lookup "title";
          cast = lookup "cast";
          crew = lookup "crew"} : Movie.credit ) )

(*
   read rating data from ratings_small.csv, and create a rating record list
   @param: string - file name
   @ret: rating list 
*)
let parse_rating (f: string): Rating.t =
  match Csv.load f with
  | [] -> assert false
  | header :: data ->
      Csv.associate header data |>
      List.map ~f:(fun row ->
        let lookup key = List.Assoc.find_exn row ~equal:String.equal key in
        ({ userid = lookup "userId" |> int_of_string; 
           movieid = lookup "movieId" |> int_of_string;
           rating = lookup "rating" |> float_of_string } : Rating.rating ))

(*
   read movies from ratings_small.csv, and create a rating record list
   @param: string -file name
   @ret: movie list 
*)
let parse_movies (f: string): Movie.basic_movie list =
  match Csv.load f with
  | [] -> assert false
  | header :: data ->
     Csv.associate header data |>
     List.map ~f:(fun row ->
       let lookup key = List.Assoc.find_exn row ~equal:String.equal key in
        match float_of_string (lookup "vote_count") with
        | exception (Failure _) -> None
        | _ ->
          Some ({ 
          genres = lookup "genres";
          homepage = lookup "homepage";
          id = lookup "id" |> int_of_string;
          keywords = lookup "keywords";
          overview = lookup "overview";
          popularity = lookup "popularity" |> float_of_string;
          release_date = lookup "release_date";
          title = lookup "title";
          vote_average = lookup "vote_average" |> float_of_string;
          vote_count = lookup "vote_count" |> int_of_string }: Movie.basic_movie)) |>
    List.filter_map ~f:(function x -> x)

(*let parse_keyword (f: string): Movie.keywords =
  match Csv.load f with
  | [] -> assert false
  | header :: data ->
      Csv.associate header data |>
      List.map ~f:(fun row ->
        let lookup key = List.Assoc.find_exn row ~equal:String.equal key in
        ({ id = lookup "id" |> int_of_string; 
          keyword = lookup "keywords"}:Movie.keyword))


let lookup_keyword (keywords: Movie.keywords) (id: int): string = 
  let value = List.find_exn keywords ~f:(fun k -> k.id = id) in
  value.keyword*)

(*
   parse a json string, and  get the value of the corresponding field
   @param: string - field's value,  string - subfield's name
   @ret: string list - subfield's value
*)
let find_list (str:string) (key:string) :string list= 
  match Basic.from_string str with
  | `List l ->
      List.map l ~f:(function
        | `Assoc m -> 
            begin match List.Assoc.find_exn m ~equal:String.equal key with
            | `String s -> s
            | _ -> assert false
            end
        | _ -> assert false)
  | _ -> assert false

(*
   Parse the json string and obtain the value of the corresponding field while meeting two conditions
   @param: string - field's value,  (string * string) - subfield's name and value,    string - subfield's name
   @ret: string option - subfield's value or none
*)
let find_opt (str:string) (k, v) (name:string) : string option =
  let map = 
    match Basic.from_string str with
    | `List l ->
        List.find l ~f:(function
        | `Assoc m ->
            begin match List.Assoc.find_exn m ~equal:String.equal k with
            | `String s when String.equal s v -> true
            | _ -> false
            end
        | _ -> assert false)
    | _ -> None
  in
  match map with
  | None -> None
  | Some (`Assoc map) -> 
      begin match List.Assoc.find_exn map ~equal:String.equal name with
      | `String s -> Some s
      | _ -> None
      end
  | Some _ -> None

(*
   utilize CSV files to read multiple table data and generate movie record list
   @param: string - file name, string - file name
   @ret: movie record list
*)  
let load_movie_data(credits: string) (movies: string)(*(keywords: string)*): Movie.t =
  let credits = parse_credit credits in
  let movies = parse_movies movies in
  (*let keywords = parse_keywords keywords in*)
  let movies_map = List.fold
    ~f:(fun acc ({ id; _ } as movie) -> Map.add_exn acc ~key:id ~data:movie) 
    ~init:(Map.empty (module Int)) movies
  in
  let result = 
    List.map credits ~f:(fun { movie_id; title; cast; crew } ->
      let cast = find_list cast "name" in
      let movie = Map.find_exn movies_map movie_id in
      let genres = find_list movie.genres "name" in
      let keywords = find_list movie.keywords "name" in
      (*let keyword = find_list (lookup_keyword keywords movie_id) "name" in*)
      match find_opt crew ("job", "Director") "name" with
      | None -> None
      | Some director ->
          Some ({ movie_id = movie_id; title; cast; director; 
          keywords = keywords; genres; overview = movie.overview; 
          popularity = movie.popularity; vote_count = movie.vote_count; 
          vote_average = movie.vote_average }: Movie.movie) )
  in
  List.filter_map result ~f:(fun x -> x)

let load_rating_data (filename: string): Rating.t =
  match Csv.load filename with
  | [] -> assert false
  | header :: data ->
      Csv.associate header data |>
      List.map ~f:(fun row ->
        let lookup key = List.Assoc.find_exn row ~equal:String.equal key in
        { userid = lookup "userId" |> int_of_string; 
          movieid = lookup "movieId" |> int_of_string;
          rating = lookup "rating" |> float_of_string })
