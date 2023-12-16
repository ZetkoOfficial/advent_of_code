module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  module II = struct
    type t = int * int
    let compare = compare
  end
  module IIMap = Map.Make(II)

  type grid_elem = Rock | Wall | Empty
  type p_in = (grid_elem list) list

  let compare elem1 elem2 = 
    match elem1, elem2 with
    | Rock, Rock -> 0  | Empty, Empty -> 0
    | Rock, Empty -> 1 | Empty, Rock -> -1
    | _ -> 0
  ;;

  let from_char char = 
    match char with
    | 'O' -> Rock | '#' -> Wall | '.' -> Empty
    | _ -> failwith "invalid symbol"
  ;;

  let parse in_channel = 
    in_channel |> Solution.read_lines |> List.map (fun line -> List.map (from_char) (List.of_seq @@ String.to_seq line))
  ;;

  let split_by_wall slice = 
    let acc,rem = List.fold_right (fun char (acc,rem) ->
      if char = Wall then rem::acc, []
      else acc, char::rem
    ) slice ([],[]) in
    rem::acc
  ;;

  let sort_split rev slices = 
    if rev then 
      List.map (fun slice -> List.rev @@ List.sort compare slice) slices
    else 
      List.map (fun slice -> List.sort compare slice) slices
  ;;

  let reconstruct slices = 
    List.tl @@ List.fold_left (fun slice acc -> slice @ Wall::acc) [] slices;;
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

  let calc_weight do_transpose input =
    let input = if do_transpose then Solution.transpose input else input in 
    List.fold_left (fun acc col ->
      let acc',_ = List.fold_left (fun (acc,i) char -> 
        if char = Rock then acc+i,i+1
        else acc,i+1  
      ) (acc, 1) (List.rev col) in
      acc'
    ) 0 input
  ;;

  let rec find_cycle i map input = 
    let hash = calc_weight true input, calc_weight false input in
    match IIMap.find_opt hash map with
    | None -> find_cycle (i+1) (IIMap.add hash i map) (move_cycle input)
    | Some i' -> i', (i-i'), map
  ;;

  let solve1 input = 
    [ move Up input |> calc_weight true |> string_of_int ]
  ;;

  let solve2 input = 
    let start,cycle,map = find_cycle 1 IIMap.empty input in
    let index = start + (1000000000-start+1) mod cycle in
    let (h,_), _ = List.find (fun (_, i) -> i=index) (IIMap.bindings map) in
    
    [string_of_int h]
  ;;
  
end