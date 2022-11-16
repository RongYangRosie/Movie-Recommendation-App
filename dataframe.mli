(* A Pandas-like data frame module *)

(* Include the functionalities of Csv module *)
include module type of Csv

(* Module type *)
type t = string list list

(* Get the index of certain column by its name *)
val get_idx_by_col_name : string -> t -> int 

(* Get data of a column by the column name *)
val get_data_by_col_name : string -> t -> string list

(* Get data of certian columns by column name list *)
val get_data_by_col_names: string list -> t -> string list list

(* Get the first matched row data whose column value is equal to the input one 
   arg: col_name - string, col_value - string, dataset table - t
   ret: matched row data *)
val get_data_by_col_value: string -> string -> t -> string list 

(* Sort dataset table by column value *)
val sort_by_col_name: string -> reverse:(bool) -> t -> t

(* Add one column for all rows 
   arg: dataset table - t, col_name - string, col_values - string list
   ret: new dataset table
*)
val add_col: string -> string list -> t -> t

(* Add one row at the end of the dataset table *)
val add_row: string list -> t -> t

(* Get the first n rows *)
val top_n : int -> t -> t

(* Split the whole dataset into n fold *)
val split_n: int -> t -> t list

(* Get the size of the dataset, number of rows and columns *)
val size: t -> int * int

(* converts the input row into an assoc list which maps column header to data cell 
   arg: header list - string list, row data -> string list 
   ret: assoc list   
*)
val associate_i: string list -> string list -> (string * string) list