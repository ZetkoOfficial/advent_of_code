module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  module II = struct
    type t = int * int
    let compare = compare
  end
  module IIMap = Map.Make(II)
  module IISet = Set.Make(II)
  type p_in = char IIMap.t * (int*int) 

  let enum list =
    let (_,map) = List.fold_left (fun (i,map) arr -> 
      let (_,map') = List.fold_left (fun (j,map) char -> 
        (j+1), IIMap.add (i,j) char map
      ) (0, map) arr in
      i+1, map'
    ) (0, IIMap.empty) list in map
  ;;

  let parse in_channel = 
    let map = in_channel |> Solution.read_lines |> 
    List.map (fun line -> List.of_seq @@ String.to_seq line) |> enum in
    let (start,_) = List.find (fun (_,c) -> c = 'S') @@ IIMap.bindings map in

    (map, start)
  ;;

  let get_adj (i,j) = [
    (i,j+1,['-';'J';'7']); (i+1,j,['|';'L';'J']); 
    (i-1,j,['|';'7';'F']); (i,j-1,['L';'-';'F']);
  ];;

  let connecting = [
    ('|', [(-1,0);(+1,0)]);
    ('-', [(0,-1);(0,+1)]);
    ('L', [(-1,0);(0,+1)]);
    ('J', [(-1,0);(0,-1)]);
    ('7', [(0,-1);(+1,0)]);
    ('F', [(0,+1);(+1,0)]);
  ];;

  let start_to_points (si,sj) map =
    match List.filter ((<>) None) @@ 
    List.map (fun (x,y,valid) -> 
      match IIMap.find_opt (x,y) map with
      | None -> None
      | Some c -> if List.mem c valid then Some (x,y) else None 
    ) (get_adj (si,sj)) with

    | [Some u; Some v] -> (u,v)
    | _ -> failwith "multiple points"
  ;;

  let start_char (i,j) map = 
    let (i1,j1),(i2,j2) = start_to_points (i,j) map in
    let p1,p2 = (i1-i, j1-j), (i2-i,j2-j) in
    let c, _ = List.find (fun (_, arr) -> arr = List.sort (compare) [p1;p2]) connecting in
    c
  ;;

  let step (i,j) prev map =
    let pos = List.map (fun (y,x) -> (y+i,x+j)) 
    (List.assoc (IIMap.find (i,j) map) connecting) in

    match List.filter ((<>) prev) pos with
    | [next] -> next
    | _ -> failwith "two possible"
  ;;

  let meet map start = 
    let rec aux p1 p2 c1 c2 map cnt path1 path2 = 
      if c1 = c2 then cnt, (List.rev path1) @ (c1::path2)
      else 
        aux c1 c2 (step c1 p1 map) (step c2 p2 map) map (cnt+1) (c1::path1) (c2::path2)  in
    
    let (c1,c2) = start_to_points start map in
    aux start start c1 c2 map 1 [start] []
  ;;
  
  let is_transition prev cur = (* če gremo iz preko F7 ali LJ se ne spremeni *)
    match prev,cur with
    | 'F', 'J' -> true  | 'F', '7' -> false
    | 'L', 'J' -> false | 'L', '7' -> true 
    | _ -> failwith "unsupported"
  ;;
  
  let area map pathset = (* sproti sledimo ali smo notri ali zunaj *)
    let rec aux i j inside prev cnt = 
      if IIMap.find_opt (i,0) map = None then cnt
      else 
        match IIMap.find_opt (i,j) map with
        | None -> aux (i+1) 0 false None cnt
        | Some _ when not (IISet.mem (i,j) pathset) -> aux i (j+1) inside prev (if inside then cnt+1 else cnt)
        | Some '|' -> aux i (j+1) (not inside) None cnt
        | Some '-' -> aux i (j+1) inside prev cnt
        | Some c -> begin
          match prev with 
          | None -> aux i (j+1) (inside) (Some c) cnt
          | Some prev -> aux i (j+1) (if is_transition prev c then not inside else inside) None cnt
        end in

    aux 0 0 false None 0
  ;;
  
  let solve1 (map, start) = 
    let dist,_ = meet map start in

    [ string_of_int dist ]
  ;;
  
  let solve2 (map, start) = 
    let _,path = meet map start in 
    let char = (start_char start map) in
    let map,path = IIMap.add start char map, IISet.add_seq (List.to_seq path) IISet.empty in

    [ string_of_int @@ area map path ]
  ;;

end