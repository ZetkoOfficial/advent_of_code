module Dan1 : Solution.GenericSolution = struct 
  include Solution.StringSolution

  let solve1 input = input;;
  let solve2 input = input;;
end;;

module Dan2 : Solution.GenericSolution = struct 
  include Solution.StringSolution
  
  let solve1 input = List.map (fun s -> "|-> " ^ s) input;;
  let solve2 input = input;;
end;;

let run_test m = 
  let (module Solve): (module Solution.GenericSolution) = m in 
  Solve.write1 Stdlib.stdout (Stdlib.open_in "in/example" |> Solve.parse1 |> Solve.solve1);
  print_endline "--------------------------"
;;

let arr: (module Solution.GenericSolution) list = [(module Dan1); (module Dan2)];;
List.map run_test arr;;