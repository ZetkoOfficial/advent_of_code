module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  module SMap = Map.Make(String)
  module SSet = Set.Make(String)
  type modul_state = FlipFlop of bool | Conjuction of (string*bool) list | None of bool
  type modul = { state: modul_state; connections: string list } 
  type p_in = modul SMap.t

  let parse_line map line =
    match Str.split (Str.regexp " -> ") line with
    | [name_str;conn_str] when String.starts_with ~prefix:"%" name_str ->
      SMap.add (Str.string_after name_str 1) 
      {state=FlipFlop false; connections=(Str.split (Str.regexp ", ") conn_str)} map

    | [name_str;conn_str] when String.starts_with ~prefix:"&" name_str ->
      SMap.add (Str.string_after name_str 1) 
      {state = (Conjuction []); connections=(Str.split (Str.regexp ", ") conn_str)} map

    | [name_str;conn_str]  ->
      SMap.add name_str
      {state=None false; connections=(Str.split (Str.regexp ", ") conn_str)} map

    | _ -> failwith "invalid input"
  ;;

  let insert_prev list prev = List.map (fun a -> (a,prev)) list;;
  let insert_prev_str list prev str = List.map (fun a -> (a,prev,str)) list;;
  let update_state list prev state' = List.map (fun (name,value) -> if name=prev then (name,state') else (name,value) ) list;;
  let is_high list = List.for_all (fun (_,value) -> value = true) list;;

  let rec init_conjunctions map todo seen =
    match todo with
    | [] -> map
    | (h,prev)::t ->
      let skip = SSet.mem h seen in
      let seen = SSet.add h seen in

      match SMap.find_opt h map with
      | None -> 
        let map' = SMap.add h { state=None false; connections=[] } map in
        init_conjunctions map' t seen
      | Some m ->
        match m.state with
        | Conjuction list when not (List.mem_assoc prev list) ->
          let map' = SMap.add h {m with state = Conjuction ((prev,false)::list) } map in
          if skip then init_conjunctions map' t seen
          else 
          init_conjunctions map' (t @ insert_prev m.connections h) seen
        | _ -> 
          if skip then init_conjunctions map t seen
          else init_conjunctions map (t @ insert_prev m.connections h) seen
  ;;

  let parse in_channel = 
    let map = in_channel |> Solution.read_lines |> List.fold_left (parse_line) SMap.empty in
    init_conjunctions map ["broadcaster","button"] SSet.empty
  ;;

  let rec send_pulse map todo (low,high)= 
    match todo with
    | [] -> map, (low,high)
    | (h,prev,str)::t -> begin
      let (low,high) = if str then (low,high+1) else (low+1,high) in

      let m = SMap.find h map in
      match m.state, str with
      | FlipFlop _, true -> send_pulse map t (low,high)
      | FlipFlop false, false -> 
        let map' = SMap.add h {m with state=FlipFlop true} map in
        send_pulse map' (t @ insert_prev_str m.connections h true) (low,high)
      | FlipFlop true, false -> 
        let map' = SMap.add h {m with state=FlipFlop false} map in
        send_pulse map' (t @ insert_prev_str m.connections h false) (low,high)
      | Conjuction list, state ->
        let list' = update_state list prev state in
        let map' = SMap.add h {m with state=Conjuction list'} map in
        if is_high list' then send_pulse map' (t @ insert_prev_str m.connections h false) (low,high)
        else send_pulse map' (t @ insert_prev_str m.connections h true) (low,high)
      | None _, state ->
        let map' = SMap.add h {m with state=None state} map in
        send_pulse map' (t @ insert_prev_str m.connections h state) (low,high)
    end
  ;;

  let rec count_pulses n map (l,h)= 
    if n = 0 then (l,h)
    else 
      let map', cnt' = send_pulse map ["broadcaster","button",false] (l,h) in
      count_pulses (n-1) map' cnt'
  ;;

  let solve1 map = 
    let (low,high) = count_pulses 1000 map (0,0) in

    [ string_of_int (low * high) ]
  ;;

  let solve2 _ = ["207787533680413"; "postopek reševanja v day20.pdf"];;
end