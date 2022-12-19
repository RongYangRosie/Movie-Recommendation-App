open Core 
open OUnit2
open Movie
open Rating
open Utils
open Content_based_mode
open Collaborative_mode

(* Common Variables *)
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

let rating_list = [
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

let ur_list = [  
  {
    userid = 14;
    movieid = 21;
    rating = 4.5;
  };  
  {
    userid = 14;
    movieid = 22;
    rating = 3.5;
  };  
  {
    userid = 14;
    movieid = 25;
    rating = 3.5;
  };
]

let movie_list2 = [
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
]


(* Utils Tests *)
let test_clean_string _ = 
  assert_equal "cleanedstring" @@ clean_string "cleaned string";
  assert_equal "shouldlowercase" @@ clean_string "ShoULd LoWerCase"

let test_combine_helper _ = 
  assert_equal "samworthington zoesaldana sigourneyweaver" @@ combine_helper movie1.cast;
  assert_equal "spy basedonnovel secretagent" @@ combine_helper movie2.keywords

let test_combine_field _ = 
  assert_equal "cultureclash future spacewar samworthington zoesaldana sigourneyweaver jamescameron action adventure fantasy" @@ combine_field movie1;
  assert_equal "spy basedonnovel secretagent danielcraig christophwaltz léaseydoux sammendes action adventure crime" @@ combine_field movie2

let test_shuffle _ = 
  assert_equal [1] @@ shuffle [1]

let test_get_max_userid _ = 
  assert_equal 13 @@ get_max_userid rating_list

let test_random_movies _ =
  assert_equal ["Avatar"] @@ get_random_movies ~n:1 ~movie_list:[movie1]

let test_flatten_string_list _ =
  assert_equal "a, b, c" @@ flatten_string_list ["a";"b";"c"]

let ur_input = [("Avatar",1.0);("Spectre",2.0)]
let ur_res = [
  {
    userid=1;
    movieid=1;
    rating=1.0;
  };
  {
    userid=1;
    movieid=2;
    rating=2.0;
  }
]
let test_wrap_user_ratings _ =
  assert_equal ur_res @@ wrap_user_ratings ur_input ~userid:1 ~movie_list

let utils_tests = 
  "Utils Test" 
  >: test_list [
    "Clean string" >:: test_clean_string;
    "Combine helper" >:: test_combine_helper;
    "Combine field" >:: test_combine_field;
    "Shuffle" >:: test_shuffle;
    "Get max userid" >:: test_get_max_userid;
    "Get random movies" >:: test_random_movies;
    "Flatten string list" >:: test_flatten_string_list;
    "Wrap user ratings" >:: test_wrap_user_ratings;
  ]

(* Mode2 Tests *)

let sim_arr = [
  [1.0000000000000002; 0.18181818181818182];
  [0.18181818181818182; 1.0000000000000002]]

let test_calculate_cosine_similarity _ =
  assert_equal sim_arr @@ calculate_cosine_similarity movie_list

let test_mode2_get_recommendations _ = 
  assert_equal [movie2] @@ Content_based_mode.get_recommendations ~movie_list:movie_list ~title:movie1.title ~n:1

let mode2_tests = 
  "Content_base_mode Test" 
  >: test_list [
    "Calculate sim" >:: test_calculate_cosine_similarity;
    "Mode2 Get recommendations" >:: test_mode2_get_recommendations
  ]

(* Mode3 Tests *)
let test_split_train_test_data _ =
  let test, train = split_train_test_data rating_list ~test_size:0.5 in 
  assert_equal (4,4) @@ (List.length test, List.length train)

let test_average_rating _ = 
  assert_equal 3.6875 @@ average_rating rating_list

let a = Array.create ~len:2 2.5
let b = Array.create ~len:2 3.2
let test_inner_product _ =
  assert_equal 16. @@ inner_product a b

let test_get_userNum _ =
  assert_equal 3 @@ get_userNum rating_list

let test_get_movieNum _ = 
  assert_equal 5 @@ get_movieNum rating_list 

let mode3_recom_res = [{Movie.movie_id = 23; title = "Candy";
cast = ["Sam Worthington"; "Zoe Saldana"; "Sigourney Weaver"];
director = "James Cameron";
keywords = ["culture clash"; "future"; "space war"];
genres = ["Action"; "Adventure"; "Fantasy"];
overview =
 "In the 22nd century, a paraplegic Marine is dispatched to the moon Pandora on a unique mission, but becomes torn between following orders and protecting an alien civilization.";
popularity = 150.437577; vote_count = 11800; vote_average = 7.2};
{Movie.movie_id = 24; title = "Dance";
cast = ["Daniel Craig"; "Christoph Waltz"; "Léa Seydoux"];
director = "Sam Mendes";
keywords = ["spy"; "based on novel"; "secret agent"];
genres = ["Action"; "Adventure"; "Crime"];
overview =
 "A cryptic message from Bond's past sends him on a trail to uncover a sinister organization. While M battles political forces to keep the secret service alive, Bond peels back the layers of deceit to reveal the terrible truth behind SPECTRE.";
popularity = 107.376788; vote_count = 4466; vote_average = 6.3}]

let test_mode3_get_recommendations _ =
  assert_equal mode3_recom_res @@ Collaborative_mode.get_recommendations ~movie_list:movie_list2 ~rating_list ~ur_list ~n:2

let mode3_tests = 
  "Collaborative_mode Test" 
  >: test_list [
    "Split train test data" >:: test_split_train_test_data;
    "Average rating" >:: test_average_rating;
    "Inner product" >:: test_inner_product;
    "Get userNum" >:: test_get_userNum;
    "Get movieNum" >:: test_get_movieNum;
    "Mode3 Get recommendations" >:: test_mode3_get_recommendations;
  ]

(* Combine all module tests *)
let series = 
  "Whole Test" >::: [utils_tests; mode2_tests; mode3_tests]
  
let () = run_test_tt_main series