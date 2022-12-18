open Core 
open OUnit2
open Movie
open Demographic_mode


(* 1 *)
(* test suite for vote_average *)
let mk_result_vote vote_average =
  { movie_id = 1;
    title = "Avatar";
    cast = [];
    director = "";
    keywords = [];
    genres = [];
    overview = "";
    popularity = 1.;
    vote_count = 1.;
    vote_average;
  }
  
  let r1 = mk_result_vote 2.
  let r2 = mk_result_vote 4.
  let r3 = mk_result_vote 6.
  
  let vote_average_tests = "test suite for vote_average" >:::
    [
      "vote_average_test1" >:: (fun _ -> assert_equal 2. (vote_average [r1]));
      "vote_average_test2" >:: (fun _ -> assert_equal 3. (vote_average [r1; r2]));
      "vote_average_test3" >:: (fun _ -> assert_equal 4. (vote_average [r1; r2; r3]));
    ]
  (* test suite for vote_average *)
  
  (* 2 *)
  (* test suite for quantile *)
  let data = [{Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 1.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 2.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 3.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 4.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 5.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 6.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 7.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 8.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 9.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 10.;
  vote_average = 1.}]
  
  let quantile_tests = "test suite for quantile" >::: [
    "quantile test1" >:: (fun _ -> assert_equal 9.1 (Demographic_mode.quantile data))
  ]
  (* test suite for quantile *)
  
  (* 3 *)
  (* test suite for filter_vote_count *)
  let result = [{Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 10.;
  vote_average = 1.}]
  let test_for_filter_vote_count = "test suite for filter_vote_count" >:::
  [
    "test filter_vote_count 1" >:: (fun _ -> assert_equal result (Demographic_mode.filter_vote_count data))
  ]
  (* test suite for filter_vote_count *)
  
  (* 4 *)
  (* test suite for sort_by_weighted_rating *)
  let result = [{Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 10.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 7.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 6.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 5.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 4.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 3.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 2.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 1.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 9.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 8.;
  vote_average = 1.}]
  let test_for_sort_by_weighted_rating = "test suite for sort_by_weighted_rating" >:::
  [
    "test sort_by_weighted_rating 1" >:: (fun _ -> assert_equal result (Demographic_mode.sort_by_weighted_rating data))
  ]
  (* test suite for sort_by_weighted_rating *)
  
  (* 5 *)
  (* test suite for sort_by_popularity *)
  let mk_result_popularity movie_id popularity =
    { movie_id;
      title = "Avatar";
      cast = [];
      director = "";
      keywords = [];
      genres = [];
      overview = "";
      popularity;
      vote_count = 1.;
      vote_average = 1.
    }
  
  let r1 = mk_result_popularity 1 1.2
  let r2 = mk_result_popularity 2 3.2
  let r3 = mk_result_popularity 3 4.2
  
  let sort_by_popularity_tests = "test suite for sort_by_popularity" >:::
    [
      "sort_by_popularity_test1" >:: (fun _ -> assert_equal [r3; r2; r1] (sort_by_popularity [r1; r2; r3]));
    ]
  (* test suite for sort_by_popularity *)
  
  (* 6 *)
  (* test suite for sort *)
  let mk_result_sort movie_id popularity vote_count =
    { movie_id;
      title = "Avatar";
      cast = [];
      director = "";
      keywords = [];
      genres = [];
      overview = "";
      popularity;
      vote_count;
      vote_average = 1.;
    }
  
  let data = List.init 10 ~f:(fun i -> mk_result_sort i (float_of_int i) (float_of_int i))
  let result = [{Movie.movie_id = 0; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 0.; vote_count = 0.;
  vote_average = 1.};
  {Movie.movie_id = 1; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 1.; vote_count = 1.;
  vote_average = 1.};
  {Movie.movie_id = 2; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 2.; vote_count = 2.;
  vote_average = 1.};
  {Movie.movie_id = 8; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 8.; vote_count = 8.;
  vote_average = 1.};
  {Movie.movie_id = 3; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 3.; vote_count = 3.;
  vote_average = 1.};
  {Movie.movie_id = 9; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 9.; vote_count = 9.;
  vote_average = 1.};
  {Movie.movie_id = 4; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 4.; vote_count = 4.;
  vote_average = 1.};
  {Movie.movie_id = 5; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 5.; vote_count = 5.;
  vote_average = 1.};
  {Movie.movie_id = 6; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 6.; vote_count = 6.;
  vote_average = 1.};
  {Movie.movie_id = 7; title = "Avatar"; cast = []; director = "";
  keywords = []; genres = []; overview = ""; popularity = 7.; vote_count = 7.;
  vote_average = 1.}]
  
  let test_for_sort = "test suite for sort" >::: [
    "test sort 1" >:: (fun _ -> assert_equal result (Demographic_mode.sort data))
  ]
  
  (* test suite for sort *)
  
  
  (* 7 *)
  (* test suite for sort_by_popularity *)
  let mk_result_nth movie_id =
    { movie_id;
      title = "Avatar";
      cast = [];
      director = "";
      keywords = [];
      genres = [];
      overview = "";
      popularity = 1.;
      vote_count = 1.;
      vote_average = 1.
    }
  
  let r1 = mk_result_nth 1
  let r2 = mk_result_nth 2
  let r3 = mk_result_nth 3
  let r4 = mk_result_nth 4
  let r5 = mk_result_nth 5
  let r6 = mk_result_nth 6
  
  let nth_tests = "test suite for sort_by_popularity" >::: [
    "nth_test1" >:: (fun _ -> assert_equal [r1; r2; r3] (get_recommendations [r1; r2; r3; r4; r5; r6] 3))
  ]
  (* test suite for sort_by_popularity *)
  
  let tests = "all the tests" >::: 
    [vote_average_tests; 
     sort_by_popularity_tests; 
     nth_tests; 
     quantile_tests; 
     test_for_filter_vote_count;
     test_for_sort_by_weighted_rating;
     test_for_sort
  ]
  
  let _ = run_test_tt_main tests
