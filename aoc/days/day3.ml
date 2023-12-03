module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution 
  
  type number = {
    value: int; edges: (int*int) list
  }
  module II = struct
    type t = int * int
    let compare = compare
  end
  module IIMap = Map.Make(II)
  type p_in = number list * char IIMap.t

  let get_adj (x,y) =
    [(x+1,y+1);(x,y+1);(x-1,y+1);
    (x+1,y); (x-1,y); 
    (x+1,y-1);(x,y-1);(x-1,y-1)]
  ;;

  let insert_all list map =
    List.fold_left (fun acc (x,y,c) -> IIMap.add (x,y) c acc) map list
  ;;

  let parse_line line j (nums,symbols) =
    let rec aux list p acc_nums acc_symbols i j= 
      match (list,p) with
      | [],None -> acc_nums,acc_symbols
      | [],Some (v,arr) -> {value=v;edges=arr}::acc_nums,acc_symbols
      | '.'::t,None -> aux t None acc_nums acc_symbols (i+1) j
      | '.'::t,Some (v,arr) -> aux t None ({value=v;edges=arr}::acc_nums) acc_symbols (i+1) j
      | h::t, None -> 
        if h >= '0' && h <= '9' then 
          aux t (Some (Char.code h - Char.code '0', get_adj (i,j))) acc_nums acc_symbols (i+1) j 
        else aux t None acc_nums ((i,j,h)::acc_symbols) (i+1) j
      | h::t, Some (v,arr) -> 
        if h >= '0' && h <= '9' then 
          aux t (Some(10*v + Char.code h - Char.code '0', get_adj(i,j)@arr)) acc_nums acc_symbols (i+1) j
        else
          aux t None ({value=v;edges=arr}::acc_nums) ((i,j,h)::acc_symbols) (i+1) j in
    let (nums',symbols') = aux (line |> String.to_seq |> List.of_seq) None [] [] 0 j in
  
    nums@nums', insert_all symbols' symbols
  ;;

  let parse_lines input = 
    let res,_ = List.fold_left (fun (acc,j) line -> (parse_line line j acc),j+1) (([],IIMap.empty),0) input in
    res
  ;;

  let parse in_channel = 
    in_channel |> Solution.read_lines |> parse_lines
  ;;

  let is_valid num symbols = 
    List.fold_left (fun acc edge -> if acc then acc else IIMap.mem edge symbols) false num.edges 
  ;;

  let close_gears num symbols = 
    List.sort_uniq compare (List.filter (fun (x,y) -> IIMap.find_opt (x,y) symbols = Some '*') num.edges)
  ;;

  let insert_num_to_gear num gear gears =
    if IIMap.mem gear gears then IIMap.add gear (num::(IIMap.find gear gears)) gears
    else IIMap.add gear [num] gears
  ;;

  let create_gear_map numbers symbols = 
    List.fold_left (fun acc num -> 
      List.fold_left (fun acc' gear -> insert_num_to_gear num gear acc') acc (close_gears num symbols)
    ) IIMap.empty numbers
  ;;

  let solve1 (numbers,symbols) = 
    let valid = List.filter (fun num -> is_valid num symbols) numbers in
    
    [ string_of_int @@ List.fold_left (fun acc num -> acc + num.value) 0 valid ]
  ;;

  let solve2 (numbers,symbols) = 
    let gear_map = create_gear_map numbers symbols in
    let valid = List.filter (fun (_,list) -> List.length list = 2) (IIMap.bindings gear_map) in

    [ string_of_int @@ List.fold_left (fun acc (_,list) -> acc + (List.hd list).value * (List.hd @@ List.tl list).value) 0 valid ]
  ;;
end