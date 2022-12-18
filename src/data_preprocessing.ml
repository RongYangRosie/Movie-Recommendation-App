open Core
(*open Yojson*)
(* open Movie *)

let parse_credit (f: string): Movie.credits =
  match Csv.load f with
  | [] -> assert false
  | header :: data ->
      Csv.associate header data |>
      List.map ~f:(fun row ->
        let lookup key = List.Assoc.find_exn row ~equal:String.equal key in
        ({ id = lookup "id" |> int_of_string; 
          cast = lookup "cast";
          crew = lookup "crew"} : Movie.credit ) )

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

let parse_movies (f: string): Movie.movies =
  match Csv.load f with
  | [] -> assert false
  | header :: data ->
      Csv.associate header data |>
      List.map ~f:(fun row ->
        let lookup key = List.Assoc.find_exn row ~equal:String.equal key in
        ({ 
        budget = lookup "budget"; 
        genres = lookup "genres";
        homepage = lookup "homepage";
        id = lookup "id" |> int_of_string;
        (*keywords = lookup "keywords";*)
        original_language = lookup "original_language";
        original_title = lookup "original_title";
        overview = lookup "overview";
        popularity = lookup "popularity" |> float_of_string;
        production_companies = lookup "production_companies";
        production_countries = lookup "production_countries";
        release_date = lookup "release_date";
        revenue = lookup "revenue";
        runtime = lookup "runtime";
        spoken_languages = lookup "spoken_languages";
        status = lookup "status";
        tagline = lookup "tagline";
        title = lookup "title";
        vote_average = lookup "vote_average" |> float_of_string;
        vote_count = lookup "vote_count" |> float_of_string }: Movie.basic_movie))


(*let find_list str key = 
  let clean_str = String.tr ~target: '\'' ~replacement: '"' str in
  match Basic.from_string clean_str with
  | `List l ->
      List.map l ~f:(function
        | `Assoc m -> 
            begin match List.Assoc.find_exn m ~equal:String.equal key with
            | `String s -> s
            | _ -> assert false
            end
        | _ -> assert false)
  | _ -> assert false

let find_opt str (k, v) name =
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

let  load_movie_data(credits: string) (movies: string)(*(keywords_movie: string)*): Movie.t =
  let credits = parse_credit credits in
  let movies = parse_movies movies in
  (*let keywords = parse_keywords keywords_movie in*)
  let movies_map = List.fold
    ~f:(fun acc ({ id; _ } as movie) -> Map.add_exn acc ~key:id ~data:movie) 
    ~init:(Map.empty (module Int)) movies
  in
  let result = 
    List.map credits ~f:(fun { id;  cast; crew } ->
      let cast = find_list cast "name" in
      let movie = Map.find_exn movies_map id in
      let genres = find_list movie.genres "name" in
      (*let keyword = find_list keywords.keyword "name" in*)
      match find_opt crew ("job", "Director") "name" with
      | None -> None
      | Some director ->
          Some ({ movie_id = id; title =  movie.title; cast; director; 
          keywords = ["test1";"test2"]; genres; overview = movie.overview; 
          popularity = movie.popularity; vote_count = movie.vote_count; 
          vote_average = movie.vote_average }: Movie.movie) )
  in
  List.filter_map result ~f:(fun x -> x)
  *)


  
