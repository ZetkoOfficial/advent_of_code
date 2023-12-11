module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type p_in = (int*int) list 

  let enum list =
    let _, res = List.fold_left (fun (i,acc) line -> 
      let _, acc = String.fold_left (fun (j,acc) char -> 
        if char = '#' then (j+1),(i,j)::acc
        else j+1,acc
      ) (0,acc) line in (i+1),acc 
    ) (0,[]) list in
    res
  ;;

  let parse in_channel = in_channel |> Solution.read_lines |> enum;;

  let to_sorted_arr list = Array.of_list @@ List.sort_uniq (compare) list;;

  let dist_dir p q factor list =
    let p,q = min p q, max p q in 
    let pi,qi = Solution.bin_search p list, Solution.bin_search q list in
    let in_list = max (qi-pi-1) 0 in
    let all = max (q-p-1) 0 in
    
    (all-in_list) * factor + in_list + if p <> q then 1 else 0
  ;;

  let dist (i1,j1) (i2,j2) factor x y = 
    dist_dir i1 i2 factor x + dist_dir j1 j2 factor y
  ;; 

  let solve points factor = 
    let x,y = List.split points in
    let x,y = to_sorted_arr x, to_sorted_arr y in
    List.fold_left (fun acc p1 -> 
      List.fold_left (fun acc p2 -> 
          if p1 <= p2 then acc
          else acc + (dist p1 p2 factor x y)
      ) acc points
    ) 0 points
  ;;

  let solve1 points = [ string_of_int @@ solve points 2 ];;  
  let solve2 points = [ string_of_int @@ solve points 1000000 ];;  

end