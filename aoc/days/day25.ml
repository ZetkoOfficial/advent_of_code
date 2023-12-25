module  Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution  

  module SS = struct type t = string * string let compare = compare end

  module SMap = Map.Make(String)
  module SSMap = Map.Make(SS)
  module SSet = Set.Make(String)

  type p_in = (string list SMap.t) * (string array) 

  let add map k v = 
    match SMap.find_opt k map with
    | None -> SMap.add k [v] map
    | Some list -> SMap.add k (v::list) map
  ;;  

  let incr map k = 
    match SSMap.find_opt k map with
    | None -> SSMap.add k 1 map
    | Some v -> SSMap.add k (v+1) map
  ;;
  
  let remove (u,v) map =
    let list, list' = SMap.find u map, SMap.find v map in
    let map = SMap.add u (List.filter ((<>)v) list) map in    
    SMap.add v (List.filter ((<>)u) list') map
  ;;

  let parse_line map line =
    match Str.split (Str.regexp ": \\| ") line with
    | name::rem ->
      List.fold_left (fun acc entry -> 
        let acc = add acc name entry in
        add acc entry name 
      ) map rem
    | _ -> failwith "invalid input" 
  ;;

  let parse in_channel = 
    let map = in_channel |> Solution.read_lines |> List.fold_left (parse_line) SMap.empty in
    let arr = Array.make (SMap.cardinal map) "" in
    List.iteri (fun i (k,_) -> arr.(i) <- k) (SMap.bindings map);

    map, arr
  ;;

  let select_random arr = 
    let u,v = Random.int (Array.length arr), Random.int (Array.length arr) in    
    arr.(u), arr.(v)
  ;;

  let to_sorted (u,v) = if u < v then (u,v) else (v,u) ;;

  let path map s e = (* najdemo pot od s do e *)
    let to_do = Queue.create () in
    let rec aux seen prev =
      if Queue.is_empty to_do then prev
      else 
        let h = Queue.take to_do in
        let seen = SSet.add h seen in
        let adj = List.filter (fun v -> not (SSet.mem v seen)) (SMap.find h map) in
        let prev = List.fold_left (fun acc v -> SMap.add v h acc) prev adj in

        if List.mem e adj then prev
        else begin 
          Queue.add_seq to_do (List.to_seq adj);
          aux seen prev 
        end in 

    let rec reconstruct prev cur acc =
      if cur = s then (cur::acc)
      else reconstruct prev (SMap.find cur prev) (cur::acc) in
    
    Queue.add s to_do;
    let prev = aux SSet.empty SMap.empty in
    let res,_ = List.fold_left (fun (acc,prev) cur -> 
      if prev = "" then (acc,cur) else (to_sorted(cur,prev)::acc),cur
      ) ([],"") (reconstruct prev e []) in
    res
  ;;

  let rec run_n map arr n occ = (* izberemo naključni dve oglišči in pot med njima *)
    if n = 0 then occ
    else 
      let u,v = select_random arr in
      let occ' = List.fold_left (fun acc v -> incr acc v) occ (path map u v) in
      run_n map arr (n-1) occ'
  ;;

  let rec dfs map to_add seen = (* dolžina enega odseka *)
    match to_add with
    | [] -> SSet.cardinal seen
    | h::t -> 
      let adj = List.filter (fun v -> not (SSet.mem v seen)) (SMap.find h map) in
      dfs map (adj @ t) (SSet.add h seen)
  ;;

  let rec try_until map arr tries = (* odstranimo najpogostejše povezave dokler ne najdemo rešitve *)
    let occ =  run_n map arr tries SSMap.empty in
    let sorted = List.map (fun ((u,v),_) -> (u,v)) @@ List.sort (fun (_,x) (_,y) -> compare y x) (SSMap.bindings occ) in

    let a,b,c = match sorted with
    | a::b::c::_ -> a,b,c
    | _ -> failwith "invalid" in

    let map' = map |> remove a |> remove b |> remove c in
    let start,_ = SMap.min_binding map in

    let size = dfs map' [start] SSet.empty in
    if size = Array.length arr then try_until map arr tries
    else (Array.length arr-size) * size
  ;;

  let solve1 (map,arr) = 
    Random.self_init ();
    [ string_of_int @@ try_until map arr 100]
  ;;

  let solve2 _ = [""];;

end