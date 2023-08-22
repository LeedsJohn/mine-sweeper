open! Core
open Stuff

type t = { board : square Array.t Array.t; rows : int; cols : int }

let place_mine t =
  let quit = ref false in
  while not !quit do
    let row, col = get_random_row_col ~rows:t.rows ~cols:t.cols in
    match t.board.(row).(col) with
    | Mine -> ()
    | Clear _ | Unknown ->
        t.board.(row).(col) <- Mine;
        quit := true
  done

let fill_clear_nums t =
  Array.iteri t.board ~f:(fun rowi row ->
      Array.iteri row ~f:(fun coli square ->
          match square with
          | Mine -> ()
          | Unknown | Clear _ ->
              t.board.(rowi).(coli) <-
                Clear
                  (count_mine_neighbors t.board ~rows:t.rows ~cols:t.cols
                     ~row:rowi ~col:coli)))

let create ~rows ~cols ~num_mines =
  let board = Array.init rows ~f:(fun _ -> Array.create ~len:cols Unknown) in
  let t = { board; rows; cols } in
  for _ = 0 to num_mines - 1 do
    place_mine t
  done;
  fill_clear_nums t;
  t

let shuffle_mine t ~row ~col =
  let quit = ref false in
  while not !quit do
    t.board.(row).(col) <- Clear (-1);
    place_mine t;
    match t.board.(row).(col) with
    | Mine | Unknown -> ()
    | Clear _ -> quit := true
  done;
  fill_clear_nums t

let get t ?(first_move = false) ~expected ~row ~col () =
  (match t.board.(row).(col) with
  | Mine -> if first_move then shuffle_mine t ~row ~col
  | Clear _ | Unknown -> ());
  match (expected, t.board.(row).(col)) with
  | Clear _, (Clear _ as real) -> `Good real
  | Mine, Mine -> `Good Mine
  | _, _ -> `Dead

let%test_unit "board creation" =
  let rows = 40 in
  let cols = 90 in
  let num_mines = 100 in
  let t = create ~rows ~cols ~num_mines in
  [%test_eq: int] (Array.length t.board) rows;
  [%test_eq: int] (Array.length t.board.(0)) cols;
  let actual_num_mines =
    Array.fold t.board ~init:0 ~f:(fun acc row ->
        acc
        + Array.count row ~f:(fun square ->
              match square with Mine -> true | Clear _ | Unknown -> false))
  in
  [%test_eq: int] actual_num_mines num_mines
