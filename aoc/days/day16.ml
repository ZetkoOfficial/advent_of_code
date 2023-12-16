module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type grid_elem = Empty | MirrorRight | MirrorLeft | SplitterX | SplitterY;;
  module II = struct
    type t = int * int
    let compare = compare
  end
  module IIMap = Map.Make(II)

  type p_in = (grid_elem IIMap.t) * int * int

  let enum list =
    let _, res = List.fold_left (fun (i,acc) line -> 
      let _, acc = String.fold_left (fun (j,acc) char -> 
        (j+1), match char with
        | '.' -> IIMap.add (i,j) Empty acc
        | '/' -> IIMap.add (i,j) MirrorRight acc | '\\' -> IIMap.add (i,j) MirrorLeft acc
        | '|' -> IIMap.add (i,j) SplitterY acc   | '-'  -> IIMap.add (i,j) SplitterX acc
        | _   -> failwith "invalid"
      ) (0,acc) line in (i+1),acc 
    ) (0,IIMap.empty) list in
    res, (List.length list), String.length (List.hd list)
  ;;

  let parse in_channel =
    in_channel |> Solution.read_lines |> enum
  ;;

  let transition ((i,j),(di,dj)) cur =
    let dir' = 
    match cur, (di,dj) with
    | Empty, _ -> [di,dj]
    | MirrorLeft,   (1,0) -> [0,1]      | MirrorLeft,   (-1,0) -> [0,-1]
    | MirrorLeft,   (0,1) -> [1,0]      | MirrorLeft,   (0,-1) -> [-1,0]
    | MirrorRight,  (1,0) -> [0,-1]     | MirrorRight,  (-1,0) -> [0,1]
    | MirrorRight,  (0,1) -> [-1,0]     | MirrorRight,  (0,-1) -> [1,0]
    | SplitterX,    (1,0) -> [0,-1;0,1] | SplitterX,    (-1,0) -> [0,-1;0,1]
    | SplitterY,    (0,1) -> [-1,0;1,0] | SplitterY,    (0,-1) -> [-1,0;1,0]
    | SplitterX, _        -> [di, dj]   | SplitterY, _         -> [di, dj]

    | _ -> failwith "invalid transition" in

    List.map (fun (di,dj) -> (i+di, j+dj), (di, dj)) dir'
  ;;

  let get_default p grid = 
    match IIMap.find_opt p grid with
    | None -> Empty
    | Some elem -> elem
  ;;

  let cull_outside (n,m) pos = 
    List.fold_left (fun acc ((i,j),d) -> if i >= n || i < 0 || j>=m || j < 0 then acc else ((i,j),d)::acc ) [] pos
  ;;

  let add_to map key value =
    match IIMap.find_opt key map with
    | None -> IIMap.add key [value] map
    | Some list -> IIMap.add key (value::list) map
  ;;

  let move grid bounds (pos,marked) = 
    let active, pos' = (List.fold_left (fun (active,acc) (p,d) -> 
      match IIMap.find_opt p marked with
      | Some seen when List.mem d seen -> active,acc
      | _ -> true,List.rev_append (transition (p,d) (get_default p grid)) acc
    ) (false,[]) pos) in
    let pos' = cull_outside bounds pos' in 

    active, (pos', List.fold_left (fun acc (p,d) -> (add_to acc p d)) marked pos)
  ;;

  let rec count_valid marked (n,m) (i,j) acc = 
    if j >= m then count_valid marked (n,m) (i+1,0) acc
    else if i >= n then acc
    else count_valid marked (n,m) (i,j+1) (if IIMap.mem (i,j) marked then 1+acc else acc) 
  ;;

  let find_length grid bounds start_p start_dir= 
    let rec aux data = 
      let active,(pos',marked') = move grid bounds data in
      if active then aux (pos',marked') 
      else count_valid marked' bounds (0,0) 0 in

    aux ([start_p,start_dir], IIMap.empty)
  ;;

  let find_max pre_max grid bounds start start_dir (ti,tj) n =
    let rec aux i (ci,cj) acc =  
      if i = 0 then acc
      else aux (i-1) (ci+ti,cj+tj) (max acc (find_length grid bounds (ci,cj) start_dir)) in

    aux n start pre_max
  ;;

  let solve1 (grid,n,m) = 
    [ string_of_int @@ find_length grid (n,m) (0,0) (0,1) ]
  ;;

  let solve2 (grid,n,m) = (* verjetno dovolj hitro samo naivno *)
    let max = find_max 0 grid (n,m) (0,0) (0,1) (1,0) n in
    let max = find_max max grid (n,m) (0,0) (1,0) (0,1) n in
    let max = find_max max grid (n,m) (n-1,m-1) (-1,0) (0,-1) n in
    let max = find_max max grid (n,m) (n-1,m-1) (0,-1) (-1,0) n in

    [ string_of_int max ]
  ;;

end