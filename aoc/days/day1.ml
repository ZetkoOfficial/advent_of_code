module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  let find input do_forward = 
    let to_replace = ["one",'1'; "two",'2'; "three",'3'; "four",'4'; "five",'5'; "six",'6'; "seven",'7'; "eight",'8'; "nine",'9'] in

    let (search_fun, comp_fun, start_i, end_i) = 
      if do_forward then  (Str.search_forward,  (<=), 0,                        String.length input)
      else                (Str.search_backward, (>=), (String.length input)-1, -1)
    in
    
    List.fold_left ( fun (i, v) (num,v') -> 
      try 
        let i' = search_fun (Str.regexp num) input start_i in
        if comp_fun i' i then (i', v') 
        else (i,v)
      with Not_found -> (i,v)
    ) (end_i, 'a') to_replace
  
  ;;

  let replace_words input = 
    List.map (fun vrstica -> 
      let (is,vs) = find vrstica true in
      let (ie,ve) = find vrstica false in
      String.mapi (fun i c -> if i = is then vs else if i = ie then ve else c) vrstica
    ) input
  ;;
  
  let filter input = 
    List.filter (fun s -> not (s = "")) (List.map (fun vrstica -> Str.global_replace (Str.regexp "[^0-9]") "" vrstica) input)
  ;;

  let extract_cal input = 
    List.map (fun str -> 
      let s = Char.code str.[0]-Char.code '0' and e = Char.code str.[(String.length str)-1]-Char.code '0' in
      10*s + e
    ) input
  ;;

  let solve1 input = 
    let parsed = input |> filter |> extract_cal in
    let sum = List.fold_left (fun acc cal -> acc+cal) 0 parsed in
    [string_of_int sum]
  ;;

  let solve2 input = 
    let parsed = input |> replace_words |> filter |> extract_cal in
    let sum = List.fold_left (fun acc cal -> acc+cal) 0 parsed in
    [string_of_int sum]
  ;;

end;;