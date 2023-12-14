module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  module II = struct
    type t = int * int
    let compare = compare
  end
  module IIMap = Map.Make(II)

  type p_in = (char list) list

  let parse in_channel = 
    in_channel |> Solution.read_lines |> List.map (fun line -> List.of_seq @@ String.to_seq line)
  ;;

  let split_by_wall slice = 
    let acc,rem = List.fold_left (fun (acc,rem) char ->
      if char = '#' then (List.rev rem)::acc, []
      else acc, char::rem
    ) ([],[]) slice in
    List.rev ((List.rev rem)::acc)
  ;;

  let sort_split rev slices = 
    if rev then 
      List.map (fun slice -> List.rev @@ List.sort compare slice) slices
    else 
      List.map (fun slice -> List.sort compare slice) slices
  ;;

  let reconstruct slices = 
    List.tl @@ List.fold_left (fun slice acc -> slice @ '#'::acc) [] slices;;
  ;;

  let move_dir rev input = List.map (fun col -> col |> split_by_wall |> sort_split rev |> reconstruct) input;;
  type direction = Left | Right | Up | Down

  let move dir input = 
    match dir with
    | Up ->   Solution.transpose @@ move_dir true (Solution.transpose input)
    | Down -> Solution.transpose @@ move_dir false (Solution.transpose input)
    | Left -> move_dir true input
    | Right-> move_dir false input 
  ;;

  let move_cycle input = 
    input |> move Up |> move Left |> move Down |> move Right
  ;; 

  let calc_weight input =
    let input = Solution.transpose input in 
    List.fold_left (fun acc col ->
      let acc',_ = List.fold_left (fun (acc,i) char -> 
        if char = 'O' then acc+i,i+1
        else acc,i+1  
      ) (acc, 1) (List.rev col) in
      acc'
    ) 0 input
  ;;

  let rec find_cycle i map input = 
    let hash = calc_weight input, calc_weight @@ Solution.transpose input in
    match IIMap.find_opt hash map with
    | None -> find_cycle (i+1) (IIMap.add hash i map) (move_cycle input)
    | Some i' -> i', (i-i'), map
  ;;

  let solve1 input = 
    [ move Up input |> calc_weight |> string_of_int ]
  ;;

  let solve2 input = 
    let start,cycle,map = find_cycle 1 IIMap.empty input in
    let index = start + (1000000000-start+1) mod cycle in
    let (h,_), _ = List.find (fun (_, i) -> i=index) (IIMap.bindings map) in
    
    [string_of_int h]
  ;;
  
end