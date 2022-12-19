open Core

type rating = {
  userid: int;
  movieid: int;
  rating: float;
} [@@deriving show]

type t = rating list [@@deriving show]

let csv2list (f: string): t =
  match Csv.load f with
  | [] -> assert false
  | header :: data ->
      Csv.associate header data |>
      List.map ~f:(fun row ->
        let lookup key = List.Assoc.find_exn row ~equal:String.equal key in
        { userid = lookup "userId" |> int_of_string; 
          movieid = lookup "movieId" |> int_of_string;
          rating = lookup "rating" |> float_of_string })