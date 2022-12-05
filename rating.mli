type rating = { userid : int; movieid : int; rating : float; }
type t = rating list
val csv2list : string -> rating list