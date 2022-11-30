let _ = 
  let result = Rating.csv2list Sys.argv.(1) in
  Rating.show result |> print_endline