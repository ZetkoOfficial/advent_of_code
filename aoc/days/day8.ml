module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution
  
  module SMap = Map.Make(String)
  type p_in = string * ((string*string) SMap.t)

  let handle_conn map line = 
    match Str.split (Str.regexp (" = (\\|, \\|(\\|)")) line with 
    | [key;left;right] -> SMap.add key (left,right) map
    | _ -> failwith "parse_error conn"
  ;;

  let parse in_channel =
    match in_channel |> Solution.read_lines with
    | ins::_::conn -> ins, List.fold_left (handle_conn) SMap.empty conn
    | _ -> failwith "parse error"
  ;;

  let is_final_1 node = node = "ZZZ";;
  let is_final_2 node = node.[String.length node - 1] = 'Z';;
  let is_start node = node.[String.length node - 1] = 'A';;

  let run_once ins map cnt s is_final = 
    String.fold_left (fun (cnt,cur) ins ->
      if is_final cur && false then (cnt,cur)
      else 
        let (l,r) = SMap.find cur map in
          match ins with
          | 'L' -> (cnt+1),l | 'R' -> (cnt+1),r
          | _ -> failwith "invalid symbol"
    ) (cnt,s) ins
  ;;

  let run_until ins map s is_final skip_first = 
    let rec aux (cnt, cur) skip_first = 
      if is_final cur && not skip_first then cnt, cur
      else aux (run_once ins map cnt cur is_final) false in
    aux (0, s) skip_first
  ;;

  let cycle_length ins map s = 
    let (cnt, _) = run_until ins map s ((=) s) true in cnt
  ;;

  let gcd a b = 
    let rec aux a b p q = 
      if a = 0 then b, 0, 1
      else 
        let (g,p,q) = aux (b mod a) a p q in
        g, q-p*(b/a), p in
    aux a b 0 0 
  ;;

  let rec pos_reduce a t m = 
    if a < t then pos_reduce (a+m) t m
    else a
  ;;

  let rec chinese_rem equations = 
    match equations with
    | [] -> failwith "error solving"
    | [(a,c)] -> (pos_reduce (a mod c) 0 c), c 
    | (a1,c1)::(a2,c2)::t -> 
      let (g, p, q) = gcd c1 c2 in
      let a,c = a1 * (q*c2)/g + a2 * (p*c1)/g, (c1*c2)/g in
      chinese_rem ((pos_reduce (a mod c) 0 c, c)::t)
  ;;

  let solve1 (ins,map) =
    let (cnt, _) = run_until ins map "AAA" is_final_1 false in

    [ string_of_int cnt ]
  ;;

  let rec _lcm list = 
    match list with
    | [] -> failwith "error"
    | [h] -> h
    | h1::h2::t -> let (g,_,_) = gcd h1 h2 in _lcm (((h1*h2)/g)::t)
  ;;

  let solve2 (ins,map) = (* predpostavimo da je v vsakem ciklu le eno veljavno končno mesto in se cikel začne z njem *)
    let start = SMap.bindings map |> List.map (fun (u,_) -> u) |> List.filter (is_start) in
    let final_points = List.map (fun s -> run_until ins map s is_final_2 false) start in
    let cycle_lengths = List.map (fun (_,s) -> cycle_length ins map s) final_points in

    let equs = List.map2 (fun (a,_) c -> (a,c)) final_points cycle_lengths in
    let max = List.fold_left (fun acc (a,_) -> max a acc) 0 final_points in

    let (a,c) = chinese_rem equs in
    [ string_of_int @@ pos_reduce (a mod c) max c ]
  ;;


end