let _ = 
  Printexc.record_backtrace true;
  let movies = Sys.argv.(1) in
  let credits = Sys.argv.(2) in
  Movie.csv2list movies credits