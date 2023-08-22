open! Core

type square = Unknown | Mine | Clear of int

val print : square Array.t Array.t -> unit

val get_random_row_col : rows:int -> cols:int -> int * int

val get_neighbors : rows:int -> cols:int -> row:int -> col:int -> (int * int) list

val count_mine_neighbors : square Array.t Array.t -> rows:int -> cols:int -> row:int -> col:int -> int
val count_clear_neighbors : square Array.t Array.t -> rows:int -> cols:int -> row:int -> col:int -> int
val count_unknown_neighbors : square Array.t Array.t -> rows:int -> cols:int -> row:int -> col:int -> int
val count_all_neighbors : square Array.t Array.t -> rows:int -> cols:int -> row:int -> col:int -> int * int * int