open! Core
open! Stuff

type t = {
  board : Board.t;
  info : square Array.t Array.t;
  rows : int;
  cols : int;
  mines_remaining : int ref;
}

val create : rows:int -> cols:int -> num_mines:int -> t

val solve : t -> bool