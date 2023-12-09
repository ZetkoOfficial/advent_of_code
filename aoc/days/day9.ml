module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution
  type p_in = (int list) list * (int list) list

  let one_diff arr = 
    let _, res = List.fold_left (fun (pre,acc) v -> v,(v-pre)::acc) (List.hd arr, []) (List.tl arr) in
    let l,r = res, List.rev res in  
    
    r, List.hd r, List.hd l
  ;;

  let to_diffs arr = 
    let rec aux arr lacc racc = 
      let arr', l, r = one_diff arr in
      if List.for_all ((=) 0) arr' then lacc, racc
      else aux arr' (l::lacc) (r::racc) in

    aux arr [List.hd arr] [List.hd @@ List.rev arr]
  ;;

  let step cur dir = 
    let rec aux cur pre acc = 
      match cur with
      | [] -> List.rev acc, List.hd acc
      | h::t -> aux t (h+dir*pre) (h+dir*pre::acc) in

    aux cur 0 []
  ;;

  let parse in_channel = 
    in_channel |> Solution.read_lines |> List.map (fun line -> 
      List.map (int_of_string) (Str.split (Str.regexp " +") line)  
    ) |> List.map (to_diffs) |> List.split
  ;;

  let solve1 (_,rdiffs) = 
    let nexts = List.map (fun diff -> let _,v = step diff (1) in v) rdiffs in

    [string_of_int @@ List.fold_left (+) 0 nexts] 
  ;;

  let solve2 (ldiffs,_) = 
    let prevs = List.map (fun diff -> let _,v = step diff (-1) in v) ldiffs in

    [string_of_int @@ List.fold_left (+) 0 prevs] 
  ;;
end