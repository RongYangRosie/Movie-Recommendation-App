open Core 
open OUnit2
open Content_based_mode
open Movie

let movie1 = {
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
}
let movie2 = {
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
}

let movie_list = [ movie1; movie2 ]

let test_clean_string _ = 
  assert_equal "cleanedstring" @@ clean_string "cleaned string";
  assert_equal "shouldlowercase" @@ clean_string "ShoULd LoWerCase"

let test_combine_helper _ = 
  assert_equal "samworthington zoesaldana sigourneyweaver" @@ combine_helper movie1.cast;
  assert_equal "spy basedonnovel secretagent" @@ combine_helper movie2.keywords

let test_combine_field _ = 
  assert_equal "cultureclash future spacewar samworthington zoesaldana sigourneyweaver jamescameron action adventure fantasy" @@ combine_field movie1;
  assert_equal "spy basedonnovel secretagent danielcraig christophwaltz léaseydoux sammendes action adventure crime" @@ combine_field movie2


let sim_arr = [
  [1.0000000000000002; 0.18181818181818182];
  [0.18181818181818182; 1.0000000000000002]]

let test_calculate_cosine_similarity _ =
  assert_equal sim_arr @@ calculate_cosine_similarity movie_list

let test_get_recommendations _ = 
  assert_equal ["Spectre"] @@ get_recommendations ~movie_list:movie_list ~title:movie1.title ~n:1

(* let test_vote_average_mean _ =  
  assert_equal Some(9.9) @@ vote_average_mean movie_list *)

let tests = 
  "Function Test" 
  >: test_list [
    "Clean string" >:: test_clean_string;
    "Combine helper" >:: test_combine_helper;
    "Combine field" >:: test_combine_field;
    "Calculate sim" >:: test_calculate_cosine_similarity;
    "Get recommendations" >:: test_get_recommendations
    (* "Get mean value of vote average" >:: test_vote_average_mean *)
  ]

let series = 
  "Content_base_mode Test" >::: [tests]
  
let () = run_test_tt_main series