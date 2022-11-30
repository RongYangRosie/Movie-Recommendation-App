type rating = 
  {
    userid: int;
    movieid: int;
    rating: float;
  } [@@deriving show]

type t = rating list [@@deriving show]

let csv2list filename =
  let csv = Csv.load filename |> Csv.to_array |> Array.to_list in
  match csv with
  | [] -> failwith "invalid csv file."
  | _ :: rest ->
      List.fold_left
        (fun acc line ->
          let userid  = line.(0) |> int_of_string in
          let movieid = line.(1) |> int_of_string in
          let rating  = line.(2) |> float_of_string in
          { userid; movieid; rating } :: acc)
        [] rest |>
      List.rev