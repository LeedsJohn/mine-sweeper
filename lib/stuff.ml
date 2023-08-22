open! Core

type square = Unknown | Mine | Clear of int

let print ar =
  Stdio.print_endline "";
  Array.iter ar ~f:(fun row ->
      Array.iter row ~f:(fun square ->
          match square with
          | Unknown -> Stdio.print_string "?"
          | Mine -> Stdio.print_string "*"
          | Clear x -> Stdio.printf "%d" x);
      Stdio.print_endline "")

let get_random_row_col ~rows ~cols = (Random.int rows, Random.int cols)

let get_neighbors ~rows ~cols ~row ~col =
  List.fold
    (List.range (max 0 (row - 1)) (min rows (row + 2)))
    ~init:[]
    ~f:(fun acc r ->
      acc @ List.init 3 ~f:(fun col_dif -> (r, col + col_dif - 1)))
  |> List.filter ~f:(fun (r, c) ->
         (r <> row || c <> col) && min r c >= 0 && r < rows && c < cols)

let count_type_neighbors ~rows ~cols ~row ~col ~f =
  List.count (get_neighbors ~rows ~cols ~row ~col) ~f

let count_mine_neighbors board ~rows ~cols ~row ~col =
  count_type_neighbors ~rows ~cols ~row ~col ~f:(fun (r, c) ->
      match board.(r).(c) with Mine -> true | _ -> false)

let count_clear_neighbors board ~rows ~cols ~row ~col =
  count_type_neighbors ~rows ~cols ~row ~col ~f:(fun (r, c) ->
      match board.(r).(c) with Clear _ -> true | _ -> false)

let count_unknown_neighbors board ~rows ~cols ~row ~col =
  count_type_neighbors ~rows ~cols ~row ~col ~f:(fun (r, c) ->
      match board.(r).(c) with Unknown -> true | _ -> false)

let count_all_neighbors board ~rows ~cols ~row ~col =
  List.fold (get_neighbors ~rows ~cols ~row ~col) ~init:(0, 0, 0)
    ~f:(fun (mine, clear, unknown) (r, c) ->
      match board.(r).(c) with
      | Mine -> (mine + 1, clear, unknown)
      | Clear _ -> (mine, clear + 1, unknown)
      | Unknown -> (mine, clear, unknown + 1))
