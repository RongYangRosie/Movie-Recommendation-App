type rating = { userid : int; movieid : int; rating : float; }
val pp_rating : Format.formatter -> rating -> unit
val show_rating : rating -> string

type t = rating list
val pp : Format.formatter -> t -> unit
val show : t -> string
