type rating = { userid : int; movieid : int; rating : float; }
type t = rating list
val csv2list : string -> rating list

val pp_rating : Format.formatter -> rating -> unit
val show_rating : rating -> string
val pp : Format.formatter -> t -> unit
val show : t -> string
