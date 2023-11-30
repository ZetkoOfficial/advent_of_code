module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  let solve1 input = input;;
  let solve2 input = List.map (fun s -> "To je izboljšan "^s) input;;
end;;