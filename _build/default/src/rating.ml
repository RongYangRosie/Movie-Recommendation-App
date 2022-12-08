type rating = 
  {
    userid: int;
    movieid: int;
    rating: float;
  } [@@deriving show]

type t = rating list [@@deriving show]