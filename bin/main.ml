open! Core
open! Mine_sweeper

let () =
  let solved = ref 0 in
  let failed = ref 0 in
  for _ = 0 to 99 do
    let solver = Solver.create ~rows:10 ~cols:10 ~num_mines:10 in
    if Solver.solve solver then solved := !solved + 1 else failed := !failed + 1
  done;
  Stdio.printf "\n\nSolved: %d Failed: %d\n" !solved !failed
