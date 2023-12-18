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
  (11, (module Days.Day11.Resitev));
  (12, (module Days.Day12.Resitev));
  (13, (module Days.Day13.Resitev));
  (14, (module Days.Day14.Resitev));
  (15, (module Days.Day15.Resitev));
  (16, (module Days.Day16.Resitev));
  (17, (module Days.Day17.Resitev));
  (18, (module Days.Day18.Resitev));
];;

let write_solve istr m = 
  let (module Solve): (module Solution.GenericSolution) = m in 

  try 
    let start_time = Unix.gettimeofday() in

    let input = Stdlib.open_in ("in/day_"^istr^".in") |> Solve.parse in
    Solve.write1 (Stdlib.open_out ("out/day_"^istr^"_1.out")) (input |> Solve.solve1);
    Solve.write2 (Stdlib.open_out ("out/day_"^istr^"_2.out")) (input |> Solve.solve2);
    
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

match Array.length Sys.argv with
| 1 -> begin 

  print_endline "[@] Izpisovanje vseh rešitev... ";
  create_outputs implementirano 0 end
  
| 2 -> 
  begin try
    let i = int_of_string @@ Sys.argv.(1) in
    let m = List.assoc i implementirano in 

    print_endline "[@] Izpisovanje izbranega dneva... ";
    let _ = write_solve (string_of_int i) m in ()

  with _ -> print_endline "[!] Neveljevna oznaka dneva" end
  | _ -> print_endline "[!] Nepravilno število argumentov" 
;;