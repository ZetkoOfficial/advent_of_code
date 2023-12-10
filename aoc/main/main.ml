let implementirano: (int*(module Solution.GenericSolution)) list = 
[
  (0, (module Days.Day0.Resitev));
  (1, (module Days.Day1.Resitev));
  (2, (module Days.Day2.Resitev));
  (3, (module Days.Day3.Resitev));
  (4, (module Days.Day4.Resitev));
  (5, (module Days.Day5.Resitev));
  (6, (module Days.Day6.Resitev));
  (7, (module Days.Day7.Resitev));
  (8, (module Days.Day8.Resitev));
  (9, (module Days.Day9.Resitev));
  (10, (module Days.Day10.Resitev));
];;

let write_solve istr m = 
  let (module Solve): (module Solution.GenericSolution) = m in 

  try 
    let start_time = Unix.gettimeofday() in

    Solve.write1 (Stdlib.open_out ("out/day_"^istr^"_1.out")) (Stdlib.open_in ("in/day_"^istr^".in") |> Solve.parse |> Solve.solve1);
    Solve.write2 (Stdlib.open_out ("out/day_"^istr^"_2.out")) (Stdlib.open_in ("in/day_"^istr^".in") |> Solve.parse |> Solve.solve2);
    
    let end_time = Unix.gettimeofday() in
    let elapsed_ms = truncate (1000.0 *. (end_time -. start_time)) in
    
    print_endline ("|----> Izpisane rešitve za dan "^istr^" (v "^ string_of_int elapsed_ms ^"ms)"); elapsed_ms
  with _ -> begin print_endline ("|      Napaka v izvajanju dan "^istr) end; 0
;;

let rec create_outputs days total_time = 
  match days with
  | (i,m)::t ->
    let time = write_solve (string_of_int i) m in 
    create_outputs t (total_time + time)

  | [] -> print_endline ("[@] Izpisani vsi dnevi"^" (v "^ string_of_int total_time ^"ms)")
;;

print_endline "[@] Izpisovanje rešitev... ";;
create_outputs implementirano 0;;