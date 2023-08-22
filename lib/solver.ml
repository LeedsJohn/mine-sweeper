open! Core
open Stuff

type t = {
  board : Board.t;
  info : square Array.t Array.t;
  rows : int;
  cols : int;
  mines_remaining : int ref;
}

(* Also fills in info *)
let guess t ~expected ~row ~col =
  let res = Board.get t.board ~expected ~row ~col () in
  (match res with
  | `Dead -> ()
  | `Good sq -> (
      t.info.(row).(col) <- sq;
      match sq with
      | Mine -> t.mines_remaining := !(t.mines_remaining) - 1
      | Clear _ | Unknown -> ()));
  res

let create ~rows ~cols ~num_mines =
  let board = Board.create ~rows ~cols ~num_mines in
  let info = Array.init rows ~f:(fun _ -> Array.create ~len:cols Unknown) in
  let row, col = get_random_row_col ~rows ~cols in
  (match
     Board.get board ?first_move:(Some true) ~expected:(Clear 0) ~row ~col ()
   with
  | `Dead -> ()
  | `Good sq -> info.(row).(col) <- sq);
  { board; info; rows; cols; mines_remaining = ref num_mines }

(* refactor *)
let guess_obvious t =
  let handle_obvious row col =
    match t.info.(row).(col) with
    | Mine | Unknown -> false
    | Clear n -> (
        let mines, _, unknowns =
          count_all_neighbors t.info ~rows:t.rows ~cols:t.cols ~row ~col
        in
        let expected =
          if unknowns = 0 then None
          else if n - mines = 0 then Some (Clear 0)
          else if n - mines = unknowns then Some Mine
          else None
        in
        match expected with
        | None -> false
        | Some expected ->
            List.iter (get_neighbors ~rows:t.rows ~cols:t.cols ~row ~col)
              ~f:(fun (r, c) ->
                match t.info.(r).(c) with
                | Clear _ | Mine -> ()
                | Unknown ->
                    let _ = guess t ~expected ~row:r ~col:c in
                    ());
            true)
  in
  let res = ref false in
  for row = 0 to t.rows - 1 do
    for col = 0 to t.cols - 1 do
      if handle_obvious row col then res := true
    done
  done;
  !res

let guess_random t =
  let quit = ref false in
  let res = ref (`Good Unknown) in
  while not !quit do
    let row, col = get_random_row_col ~rows:t.rows ~cols:t.cols in
    match t.info.(row).(col) with
    | Clear _ | Mine -> ()
    | Unknown ->
        res := guess t ~expected:(Clear 0) ~row ~col;
        quit := true
  done;
  !res

let solve t =
  let win = ref false in
  let game_over = ref false in
  while not !game_over do
    (if guess_obvious t then ()
     else
       match guess_random t with
       | `Dead ->
           win := false;
           game_over := true
       | _ -> ());
    if !(t.mines_remaining) = 0 then (
      win := true;
      game_over := true)
  done;
  !win

let%test_unit "obvious games" =
  let t = create ~rows:1 ~cols:2 ~num_mines:1 in
  let res = solve t in
  [%test_eq: bool] true res;
  let t = create ~rows:100 ~cols:100 ~num_mines:0 in
  let res = solve t in
  [%test_eq: bool] true res
