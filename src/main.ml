open Core
open Core_unix
open Data_preprocessing
open Movie
open Utils

let input_filename = "input_ratings.txt"
let movie_list = load_movie_data "./dataset/tmdb_5000_credits.csv" "./dataset/tmdb_5000_movies.csv"
let rating_list = load_rating_data "./dataset/new_ratings.csv"

(* let rating_list = [
  {
    userid = 11;
    movieid = 21;
    rating = 3.5;
  };
  {
    userid = 11;
    movieid = 23;
    rating = 4.5;
  };
  {
    userid = 11;
    movieid = 24;
    rating = 4.0;
  };
  {
    userid = 12;
    movieid = 21;
    rating = 4.0;
  };
  {
    userid = 12;
    movieid = 22;
    rating = 4.5;
  };
  {
    userid = 12;
    movieid = 25;
    rating = 1.0;
  };
  {
    userid = 13;
    movieid = 23;
    rating = 4.5;
  };
  {
    userid = 13;
    movieid = 25;
    rating = 3.5;
  };
]

let movie_list = [
  {
    movie_id = 21;
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
    movie_id = 22;
    title = "Spectre";
    cast = ["Daniel Craig"; "Christoph Waltz"; "Léa Seydoux"];
    director = "Sam Mendes";
    keywords = ["spy"; "based on novel"; "secret agent"];
    genres = ["Action"; "Adventure"; "Crime"];
    overview = "A cryptic message from Bond's past sends him on a trail to uncover a sinister organization. While M battles political forces to keep the secret service alive, Bond peels back the layers of deceit to reveal the terrible truth behind SPECTRE.";
    popularity = 107.376788;
    vote_count = 4466;
    vote_average = 6.3;
  };{
    movie_id = 23;
    title = "Candy";
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
    movie_id = 24;
    title = "Dance";
    cast = ["Daniel Craig"; "Christoph Waltz"; "Léa Seydoux"];
    director = "Sam Mendes";
    keywords = ["spy"; "based on novel"; "secret agent"];
    genres = ["Action"; "Adventure"; "Crime"];
    overview = "A cryptic message from Bond's past sends him on a trail to uncover a sinister organization. While M battles political forces to keep the secret service alive, Bond peels back the layers of deceit to reveal the terrible truth behind SPECTRE.";
    popularity = 107.376788;
    vote_count = 4466;
    vote_average = 6.3;
  };{
    movie_id = 25;
    title = "Elan";
    cast = ["Sam Worthington"; "Zoe Saldana"; "Sigourney Weaver"];
    director = "James Cameron";
    keywords = ["culture clash"; "future"; "space war"];
    genres =  ["Action"; "Adventure"; "Fantasy"];
    overview = "In the 22nd century, a paraplegic Marine is dispatched to the moon Pandora on a unique mission, but becomes torn between following orders and protecting an alien civilization.";
    popularity = 150.437577;
    vote_count = 11800;
    vote_average = 7.2;
  };
] *)

let movie_title_list = List.map movie_list ~f:(fun elt -> elt.title)

let modes = [
  "Mode 1: Demographic Mode";
  "Mode 2: Cotent-based Mode";
  "Mode 3: Collaborative Mode";
  "Exit"
]

let show_recommendations (movie_list: Movie.t) : unit =
  Printf.printf "Title\t\tGenres\t\t\tDirector\t\t\tCast\n";
  List.iter movie_list ~f:(fun elt -> Printf.printf "%s\t%s\t%s\t%s\n" elt.title (flatten_string_list elt.genres) elt.director (flatten_string_list elt.cast) )

let choose_number (): int = 
  let validate (x:string) = (
    if String.for_all ~f:Char.is_digit x then (
      let n = Int.of_string x in if n > 0 then Ok x else Error "No 0\n")
    else Error "Please enter a valid number:    "
  )
  in 
  let num = Inquire.input "Please enter a number of recommended movies:" ~validate ~default:"10"
  in Int.of_string num

let mode1_command () = 
  let n = choose_number () in 
  Demographic_mode.get_recommendations movie_list n |> show_recommendations

let mode2_choices = [
  "Choose from random movie list";
  "Input by yourself";
]

let run_mode2 ~(title: string) ~(n: int) : Movie.t = 
  Content_based_mode.get_recommendations ~title ~n ~movie_list

let rec random_select (n:int) =
  let options = get_random_movies ~n:5 ~movie_list @ ["Others"] in 
  let title = Inquire.raw_select "What's your favorite movie?" ~options ~default:0 in 
  match title with 
  | "Others" -> random_select n
  | _ -> run_mode2 ~title ~n |> show_recommendations

let mode2_command () = 
  let n = choose_number () in 
  let choice = Inquire.select "Please select the following two input modes:" ~options:mode2_choices ~default:0 in 
  match choice with 
  | "Input by yourself" -> (
    let validate (x:string) = (
      if List.mem movie_title_list x ~equal:String.(=) then Ok x else Error "No this movie. Enter again:     "
      )
    in let title = Inquire.input "Enter your favorite movie title: " ~validate ~default:"Avatar" in 
    run_mode2 ~title ~n |> show_recommendations
    )
  | "Choose from random movie list" -> random_select n
  | _ -> failwith "Shouldn't get here"

let get_input_ratings (filename: string) : (string * float) list = 
  let file = In_channel.create filename in 
  let strings = In_channel.input_lines file in 
  In_channel.close file;
  List.map strings ~f:(fun line -> 
    let t, r = String.rsplit2_exn line ~on:' ' in 
    (String.strip t, Float.of_string r)
    )

let validate_input_file (filename:string) = 
  let cdir = getcwd () in 
  let filepath = cdir ^ "/" ^ filename in 
  if Sys_unix.file_exists_exn filepath then (
    let content = get_input_ratings filename in 
    let filtered = List.filter_map content ~f:(fun (t, _) -> 
      if List.exists movie_list ~f:(fun m -> String.(m.title = t)) then None else Some t) in 
    if List.length filtered = 0 then Ok(filename) else 
      (* print_endline (String.concat ~sep:", " filtered); *)
      Error "Some movie does not exist     "
  )
  else Error "No such file.    "

let mode3_command () = 
  let filename = Inquire.input "Please input the name of your ratings: " ~validate:validate_input_file ~default:input_filename in 
  let n = choose_number () in
  let userid = 1 + get_max_userid rating_list in 
  let ur_list = get_input_ratings filename |> wrap_user_ratings ~userid ~movie_list in 
  Collaborative_mode.get_recommendations ~movie_list ~rating_list ~ur_list ~n |> show_recommendations

let rec grouped_command () = 
  let mode = Inquire.select "Please enter the recommender Mode:" ~options:modes ~default:0 in 
  match mode with 
  | "Mode 1: Demographic Mode" -> mode1_command (); grouped_command ()
  | "Mode 2: Cotent-based Mode" -> mode2_command (); grouped_command ()
  | "Mode 3: Collaborative Mode" -> mode3_command (); grouped_command ()
  | "Exit" -> ()
  | _ -> failwith "Shouldn't get here"

let () =
  grouped_command ()