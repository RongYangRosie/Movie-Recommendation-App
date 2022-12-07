open Yojson

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
    movie_id: string;
    title: string;
    cast: string;
    crew: string;
  } [@@deriving show]

type credits = credit list [@@deriving show]

type basic_movie =
  {
    genres: string;
    id: string;
    keywords: string;
    overview: string;
    popularity: string;
    title: string;
    vote_average: string;
    vote_count: string;
  } [@@deriving show]

type movies = basic_movie list [@@deriving show]


let parse_credit filename =
    let credits = Csv.(load filename |> to_array |> Array.to_list |> List.tl) in
    List.fold_left
      (fun acc line ->
        let movie_id = line.(0) in
        let title = line.(1) in
        let cast = line.(2) in
        let crew = line.(3) in
         {movie_id ; title; cast; crew} :: acc) 
      [] credits |>
    List.rev

let parse_movies filename =
  let movies = Csv.(load filename |> to_array |> Array.to_list |> List.tl) in
  List.fold_left
    (fun acc line ->
      let genres = line.(1) in
      let id = line.(3) in
      let keywords = line.(4) in
      let overview = line.(7) in
      let popularity = line.(8) in
      let title = line.(17) in
      let vote_average = line.(18) in
      let vote_count = line.(19) in
      { genres; id; keywords; overview; popularity; title; vote_average; vote_count} :: acc)
    [] movies |>
  List.rev

let csv2list movies credits =
  let credits = parse_credit credits in
  let movies = parse_movies movies in
  let movies_map = List.fold_left
    (fun acc ({ id; _ } as basic_movie) -> Hashtbl.add acc id basic_movie; acc) 
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
        let movie = Hashtbl.find movies_map movie_id in
        let genres =
          match from_string movie.genres with
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
  List.filter_map Fun.id result