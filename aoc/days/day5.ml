module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type range = {src: int; dst: int; len: int}
  type p_in = (int list) * (range list list)

  let to_sections string =
    List.map (fun s -> Str.split (Str.regexp "\n") s) 
    (Str.split (Str.regexp "\n\n") string)
  ;;

  let to_range data =
    match List.map (int_of_string) (Str.split (Str.regexp(" +")) data) with
    | [dst;src;len] -> {src;dst;len}
    | _ -> failwith "parse error range"
  ;;

  let to_ranges section = 
    match section with
    | _::data ->
      List.map to_range data
    | [] -> failwith "parse error ranges"
  ;;

  let parse in_channel = 
    let line = 
      match in_channel |> In_channel.input_line with
      | Some line -> line
      | None -> failwith "parse error" in

    (
      List.map (int_of_string) (List.tl (Str.split (Str.regexp " ") line)), 
      in_channel |> In_channel.input_all |> to_sections |> List.map (to_ranges)
    )
  ;;

  let get_mapped i ranges = 
    match List.find_opt 
    (fun range -> range.src <= i && i < range.src + range.len) ranges with
    | Some range -> range.dst + (i-range.src)
    | None -> i
  ;;

  let intersect (a,b) (x,y) =
    if a <= x && b >= y then [(a,x);(x,y)],(y,b)
    else if x <= a && y >= b then [(a,b)],(-1,-1)
    else if x > b || y < a then [],(a,b)
    else if x >= a && y >= b then [(a,x)],(x,b)
    else if x <= a && y <= b then [(a,y)],(y,b)
    else [],(-1,-1)
  ;;

  let split_by_range interval ranges = 
    let range_intervals = List.sort compare @@ List.map (fun range -> (range.src, range.src+range.len)) ranges in
    let u,v = List.fold_left (fun (split,next) interval -> let to_add,next' = intersect next interval in (split@to_add, next')) ([], interval) range_intervals in
    List.filter (fun (a,b) -> a <> b) (v::u)
  ;;

  let map_interval interval ranges = 
    List.map (fun (a,b) -> get_mapped a ranges, 1 + get_mapped (b-1) ranges) (split_by_range interval ranges)
  ;;

  let map_intervals intervals ranges = 
    List.fold_left (fun acc interval -> (map_interval interval ranges) @ acc) [] intervals
  ;;
  
  let to_intervals seeds =
    let rec aux seeds acc = 
      match seeds with
      | [] -> acc
      | start::len::t -> aux t ((start,start+len)::acc)
      | _ -> failwith "parse error intervals" in
    
    aux seeds []
  ;; 

  let solve1 (seeds, sections) = 
    let res = List.map (fun seed -> 
      List.fold_left (fun acc ranges -> get_mapped acc ranges) seed sections    
    ) seeds in

    [ string_of_int @@ List.fold_left (min) max_int res ]
  ;;
  
  let solve2 (seeds, sections) = 
    let intervals = to_intervals seeds in
    let res = List.fold_left (map_intervals) intervals sections in
    let interval_start = List.map (fun (a,_) -> a) res in

    [ string_of_int @@ List.fold_left (min) max_int interval_start ]
  ;;
end