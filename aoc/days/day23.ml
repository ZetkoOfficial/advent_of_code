module  Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  module II =     struct type t = int * int           let compare = compare end

  module IIMap = Map.Make(II)
  module IISet = Set.Make(II)

  type grid_elem = Path | Forest | Up | Down | Left | Right
  type p_in = grid_elem IIMap.t * int * int

  let enum list =
    let _,res = List.fold_left (fun (i,acc) line -> 
      let _,acc = String.fold_left (fun (j,acc) char -> 
        let acc' = match char with
        | '.' -> IIMap.add (i,j) Path acc
        | '#' -> IIMap.add (i,j) Forest acc
        | '^' -> IIMap.add (i,j) Up acc
        | 'v' -> IIMap.add (i,j) Down acc
        | '>' -> IIMap.add (i,j) Right acc
        | '<' -> IIMap.add (i,j) Left acc
        | _ -> failwith "invalid input" in
        (j+1), acc'
      ) (0,acc) line in (i+1),acc 
    ) (0,IIMap.empty) list in
    
    (res,(List.length list), String.length (List.hd list))
  ;;
  let parse in_channel = in_channel |> Solution.read_lines |> enum;;

  let get_adj_1 map (i,j) seen = 
    let adj = match IIMap.find (i,j) map with
    | Up -> [(i-1,j)] | Down -> [(i+1,j)] | Left -> [(i,j-1)] | Right -> [(i,j+1)] 
    | Path -> [(i-1,j);(i+1,j);(i,j-1);(i,j+1)] | Forest -> [] in
    
    List.filter (fun p -> let m = IIMap.find_opt p map in m <> None && m <> Some Forest && not @@ IISet.mem p seen) adj
  ;;

  let get_adj_2 map (i,j) seen = 
    let adj = match IIMap.find (i,j) map with
    | Forest -> [] | _ -> [(i-1,j);(i+1,j);(i,j-1);(i,j+1)] in
  
    List.filter (fun p -> let m = IIMap.find_opt p map in m <> None && m <> Some Forest && not @@ IISet.mem p seen) adj
  ;;

  let longest_1 grid endpoint p = 
    let rec aux p seen = 
      let adj = get_adj_1 grid p seen in
      let possible = List.map (fun n -> aux n (IISet.add p seen)) adj in
      let best = List.fold_left (max) 0 possible in
      if best = 0 && p <> endpoint then min_int else 1+best in

    aux p IISet.empty - 1
  ;;

  let find_problematic map =
    IIMap.bindings @@ IIMap.filter (fun p value -> 
      value <> Forest && List.length @@ get_adj_2 map p IISet.empty >= 3
    ) map |> List.map (fun (p,_) -> p)
  ;;

  let add k v arr = arr.(k) <- v::(arr.(k));;
  let find_index list p = 
    let _,i = List.find (fun (v,_) -> List.mem p v) list in i
  ;;

  let join map =
    let points = List.map (fun (p,_) -> p) (IIMap.bindings @@ IIMap.filter (fun _ value -> value <> Forest) map) in
    let problematic = find_problematic map in
    let non_problematic = List.filter (fun p -> not (List.mem p problematic)) points in
    let q = IISet.add_seq (List.to_seq non_problematic) IISet.empty in
    let seen = IISet.add_seq (List.to_seq problematic) IISet.empty in

    let rec find_all to_add q seen = 
      match to_add with
      | [] -> q, seen |> IISet.to_seq |> List.of_seq |> List.filter (fun v -> not (List.mem v problematic))
      | h::t -> 
        let adj = get_adj_2 map h seen in
        find_all (adj @ t) (IISet.remove h q) (IISet.add h seen) in

    let rec aux q acc = 
      if q = IISet.empty then acc
      else 
        let p = IISet.min_elt q in let q = IISet.remove p q in
        let q,to_add = find_all [p] q seen in
        aux q (to_add::acc) in
    
    problematic, aux q []
  ;;

  let prettify map (n,m) = 
    let problematic, sections = join map in
    let enum = (fun start list -> List.mapi (fun i v -> v,i+start) list) in
    let ep, es = enum 0 problematic, enum (List.length problematic) sections in
    
    let adj =   Array.make (List.length ep + List.length es) [] in
    let dist =  Array.make (List.length ep + List.length es) 1 in

    List.iter (fun (p,i) -> 
      let adj' = get_adj_2 map p IISet.empty in
      let i's = List.sort_uniq (compare) @@ List.map (find_index es) adj' in
      List.iter (fun i' -> add i i' adj; add i' i adj) i's
    ) ep;
    List.iter (fun (p,i) -> dist.(i) <- List.length p) es;

    let s,e = find_index es (0,1), find_index es (n-1,m-2) in
    adj, dist, s, e
  ;;

  let find_skip arr k seen = List.filter (fun v -> not seen.(v)) arr.(k);;

  let longest_2 (adj,dist,s,e) =
    let seen = Array.make (Array.length adj) false in
    let rec aux p = 
      let adj = find_skip adj p seen in
      seen.(p) <- true;
      let possible = List.map (fun n -> aux n) adj in
      seen.(p) <- false;
      
      let best = List.fold_left (max) 0 possible in
      if best = 0 && p <> e then min_int else dist.(p) + best in

    aux s - 1
  ;;
  
  let solve1 (grid,n,m) = [ string_of_int @@ longest_1 grid (n-1,m-2) (0,1) ];; 
  let solve2 (grid,n,m) = [ string_of_int @@ longest_2 @@ prettify grid (n,m) ];; 

end