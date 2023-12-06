module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution 
  type p_in = (string list) * (string list)
  
  let parse in_channel = 
    let lines = in_channel |> Solution.read_lines |> List.map (
      fun line -> (List.tl (Str.split (Str.regexp " +") line)) 
    ) in 

    match lines with 
    | [time;dist] -> time, dist
    | _ -> failwith "parse error"
  ;;
  
  let get_range time dist = 
    let time, dist = float_of_int time, float_of_int dist in 

    let r = 0.5 *. (time +. sqrt (time*.time -. 4.0 *. dist)) in
    let l = dist /. r in

    Float.to_int @@ max (floor (l+.1.0)) 0.0, 
    Float.to_int @@ min (ceil  (r-.1.0)) time
  ;;

  let range_len (a,b) = b - a + 1;;

  let solve1 (times, dists) = 
    let ranges = List.map2 (fun t d -> get_range (int_of_string t) (int_of_string d)) times dists in

    [ string_of_int @@ List.fold_left (fun acc (a,b) -> acc * range_len (a,b)) 1 ranges]
  ;;

  let solve2 (times, dists) = 
    let time, dist = int_of_string @@ List.fold_left (^) "" times, int_of_string @@ List.fold_left (^) "" dists in
    
    [ string_of_int @@ range_len @@ get_range time dist ]
  ;;
end