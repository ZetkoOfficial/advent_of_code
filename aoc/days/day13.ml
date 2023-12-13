module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type p_in = (char list list) list 
  
  let split_empty lines = 
    let acc, rem = List.fold_left (fun (acc,rem) line -> 
      if line = [] then (List.rev rem)::acc, []
      else acc, line::rem 
    ) ([],[]) lines in
    List.rev @@ (List.rev rem)::acc 
  ;;

  let parse in_channel = 
    in_channel |> Solution.read_lines |> List.map (fun line -> List.of_seq @@ String.to_seq line) |> split_empty
  ;;

  let transpose list =
    let rec aux list acc = 
      if list = [] then acc
      else 
        let col, rem = 
        List.fold_right(fun row (acc,rem) ->
          match row with
          | [] -> acc,[]
          | h::t -> (h::acc), (t::rem)
        ) list ([],[])  in
        aux rem (col::acc) in
    List.rev @@ List.tl @@ aux list []
  ;;

  let rec matches list1 list2 = 
    match list1,list2 with
    | [],_ -> true
    | _,[] -> true
    | h1::t1,h2::t2 ->
      if h1 = h2 then matches t1 t2
      else false
  ;;

  let find_mirror list skip = 
    let rec aux i list acc skip = 
      match list with
      | h1::h2::t -> 
        if h1 <> h2 || i = skip then aux (i+1) (h2::t) (h1::acc) skip
        else 
          if matches t acc then i
          else aux (i+1) (h2::t) (h1::acc) skip
      | _ -> 0 in
    aux 1 list [] skip
  ;;

  let flip i j list = 
    List.mapi (fun i' row -> 
      if i<>i' then row
      else  List.mapi (fun j' v ->
        if j<>j' then v
        else 
          if v = '#' then '.'
          else '#'
      ) row
    ) list
  ;;

  let find_smudge list skip = (* O(n^4) ok *)
    if list = [] then 0
    else 
      let i_max, j_max = List.length list, List.length @@ List.hd list in
      let rec aux i j = 
        if i >= i_max then 0
        else if j >= j_max then aux (i+1) 0
        else 
          let list' = flip i j list in
          let res = find_mirror list' skip in
          if res = 0 then aux i (j+1)
          else res in
      aux 0 0
  ;;


  let solve1 input = (* predpostavimo da največ ena os reflekcije v vsako smer *)
    let sum = List.fold_left (fun acc list -> 
      let mi, mj = find_mirror list 0, find_mirror (transpose list) 0 in
      acc + 100*mi + mj
    ) 0 input in

    [ string_of_int sum ]
  ;;

  let solve2 input =
    let sum = List.fold_left (fun acc list -> 
      let mi, mj = find_mirror list 0, find_mirror (transpose list) 0 in
      let sum = 100*(find_smudge list mi) + (find_smudge (transpose list) mj) in
      acc + sum
    ) 0 input in

    [ string_of_int sum ]
  ;;

end