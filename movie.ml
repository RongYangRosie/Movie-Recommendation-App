open Yojson

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
} [@@deriving show]

type t = result list [@@deriving show]

type credit =
  {
    movie_id: string;
    title: string;
    cast: string;
    crew: string;
  } [@@deriving show]

type credits = credit list [@@deriving show]

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
  } [@@deriving show]

type movies = movie list [@@deriving show]

let parse_credit filename =
    let credits = Csv.(load filename |> to_array |> Array.to_list |> List.tl) in
    List.fold_left
      (fun acc line ->
        let movie_id = line.(0) in
        let title = line.(1) in
        let cast = line.(2) in
        let crew = line.(3) in
        { movie_id; title; cast; crew } :: acc) 
      [] credits |>
    List.rev

let parse_movies filename =
  let movies = Csv.(load filename |> to_array |> Array.to_list |> List.tl) in
  List.fold_left
    (fun acc line ->
      let budget = line.(0) in
      let genres = line.(1) in
      let homepage = line.(2) in
      let id = line.(3) in
      let keywords = line.(4) in
      let original_language = line.(5) in
      let original_title = line.(6) in
      let overview = line.(7) in
      let popularity = line.(8) in
      let production_companies = line.(9) in
      let production_countries = line.(10) in
      let release_date = line.(11) in
      let revenue = line.(12) in
      let runtime = line.(13) in
      let spoken_languages = line.(14) in
      let status = line.(15) in
      let tagline = line.(16) in
      let title = line.(17) in
      let vote_average = line.(18) in
      let vote_count = line.(19) in
      { budget; genres; homepage; id; keywords; original_language; 
        original_title; overview; popularity; production_companies; production_countries ;
        release_date; revenue; runtime; spoken_languages; status; 
        tagline; title; vote_average; vote_count } :: acc)
    [] movies |>
  List.rev

let csv2list movies credits =
  let credits = parse_credit credits in
  let movies = parse_movies movies in
  let movies_map = List.fold_left
    (fun acc ({ id; _ } as movie) -> Hashtbl.add acc id movie; acc) 
    (Hashtbl.create 10) movies
  in
  let result = 
    List.map
      (fun { movie_id; title; cast; crew } ->
        let open Basic in
        let characters = 
          match from_string cast with
          | `List l ->
              List.map
                (function
                  | `Assoc m -> 
                      begin match List.assoc "name" m with
                      | `String s -> s
                      | _ -> invalid_arg "invalid input"
                      end
                  | _ -> invalid_arg "invalid input") 
                l
          | _ -> invalid_arg "invalid input"
        in
        let director =
          match from_string crew with
          | `List l ->
              List.find_opt
                (function
                | `Assoc m ->
                    begin match List.assoc "job" m with
                    | `String "Director" -> true
                    | _ -> false
                    end
                | _ -> invalid_arg "invalid input")
              l
          | _ -> invalid_arg "invalid input"
        in
        if director = None then None else
        let director =
          match Option.get director with
          | `Assoc m -> 
              begin match List.assoc "name" m with
              | `String s -> s
              | _ -> invalid_arg "invalid input"
              end
          | _ -> invalid_arg "invalid input"
        in
        let genres =
          match from_string crew with
          | `List l ->
              List.map
                (function
                  | `Assoc m ->
                      begin match List.assoc "name" m with
                      | `String s -> s
                      | _ -> invalid_arg "invalid input"
                      end
                  | _ -> invalid_arg "invalid input") l
          | _ -> invalid_arg "invalid input"
        in
        let movie = Hashtbl.find movies_map movie_id in
        let keywords =
          match from_string movie.keywords with
          | `List l ->
              List.map
                (function
                | `Assoc m ->
                    begin match List.assoc "name" m with
                    | `String s -> s
                    | _ -> invalid_arg "invalid input"
                    end
                | _ -> invalid_arg "invalid input")
                l
          | _ -> invalid_arg "invalid input"
        in
        Some { movie_id = int_of_string movie_id; title; cast = characters; director; 
          keywords = keywords; genres; overview = movie.overview; 
          popularity = float_of_string movie.popularity; vote_count = int_of_string movie.vote_count; 
          vote_average = float_of_string movie.vote_average
        })
      credits
  in
  let result = List.filter_map Fun.id result in
  show result |> print_endline