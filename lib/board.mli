open! Core
open Stuff

type t = { board : square Array.t Array.t; rows : int; cols : int }

val create : rows:int -> cols:int -> num_mines:int -> t

val get : t -> ?first_move:bool -> expected:square -> row:int -> col:int -> unit -> [`Dead | `Good of square]