module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type state = Next of string | Accept | Reject
  type operator = Less | More
  type condition = Condition of (string * (operator * int * state)) | Final of state
  type entry = (string * int) list 
  type interval = (string * (int*int)) list

  module StringMap = Map.Make(String)

  type p_in = (interval* state) list * entry list

  let parse_conditions conditions: condition list = 
    List.fold_left (fun acc condition_str -> 
      let condition_str = Str.global_replace (Str.regexp "<") " < " condition_str in
      let condition_str = Str.global_replace (Str.regexp ">") " > " condition_str in

      let cond = match Str.split (Str.regexp " \\|:") condition_str with
      | [cat;"<";value;"A"] -> Condition (cat, (Less, int_of_string value, Accept)) 
      | [cat;"<";value;"R"] -> Condition (cat, (Less, int_of_string value, Reject)) 
      | [cat;">";value;"A"] -> Condition (cat, (More, int_of_string value, Accept))  
      | [cat;">";value;"R"] -> Condition (cat, (More, int_of_string value, Reject))  
      | [cat;"<";value;workflow] -> Condition (cat, (Less, int_of_string value, Next workflow))
      | [cat;">";value;workflow] -> Condition (cat, (More, int_of_string value, Next workflow))
      | ["A"] -> Final Accept
      | ["R"] -> Final Reject
      | [workflow] -> Final (Next workflow)
      | _ -> failwith "invalid input" in cond::acc
    ) [] conditions
  ;;

  let parse_workflow map line = 
    match String.split_on_char '{' line with
    | [name; condition_str] -> begin
      match List.rev @@ Str.split (Str.regexp ",\\|}") condition_str with
      | h::t -> StringMap.add name (parse_conditions (h::t)) map 
      | _ -> failwith "invalid input"
    end
    | _ -> failwith "inavlid input"
  ;;

  let parse_entry entries line: entry list= 
    match Str.split (Str.regexp "{\\|=\\|,\\|}") line with
    | ["x";xval;"m";mval;"a";aval;"s";sval] -> [
      ("x", int_of_string xval); ("m", int_of_string mval); 
      ("a", int_of_string aval); ("s", int_of_string sval)
    ]::entries
    | _ -> failwith "invalid entry"
  ;;

  let handle_less (interval: interval) c value = 
    let (x,x') = List.assoc c interval in
    if value <= x then [], interval
    else 
      if value >= x' then interval, []
      else
      
      List.map (fun (c', o) -> if c = c' then (c', (x, min x' value)) else (c', o)) interval, 
      List.map (fun (c', o) -> if c = c' then (c', ((max x value) -1, x')) else (c', o)) interval
  ;;

  let handle_more (interval: interval) c value = 
    let (x,x') = List.assoc c interval in
    if value >= x' then [], interval
    else 
      if value <= x then interval, []
      else 
      
      List.map (fun (c', o) -> if c = c' then (c', (max x value, x')) else (c', o)) interval, 
      List.map (fun (c', o) -> if c = c' then (c', (x, (min x' value) +1)) else (c', o)) interval
  ;;

  let handle_interval interval (condition: condition) =
    if interval = [] then ([], Reject), []
    else

    match condition with
    | Final state -> (interval, state), []
    | Condition (c, (Less, value, state)) -> 
      let valid, skip = handle_less interval c value in
      (valid, state), skip
    | Condition (c, (More, value, state)) -> 
      let valid, skip = handle_more interval c value in
      (valid, state), skip
  ;;

  let rec handle_workflow workflows interval workflow = 
    let wf = StringMap.find workflow workflows in
    let _, acc = List.fold_left (fun (interval,acc) cond -> 
      match handle_interval interval cond with
      | ([], _),interval' -> interval', acc 
      | (interval_cur, Next workflow'), interval' ->
        interval', acc @ (handle_workflow workflows interval_cur workflow')
      | (interval_cur, state), interval' ->
        interval', acc @ [interval_cur, state]
    ) (interval,[]) wf in
    acc
  ;;

  let parse in_channel = (* predpostavimo da so entries tudi v intervalih [1,4000] *)
    let workflow_lines, entry_lines = in_channel |> Solution.read_lines |> Solution.split_empty in
    
    let workflows, entries = 
    List.fold_left (parse_workflow) StringMap.empty workflow_lines, 
    List.fold_left (parse_entry) [] entry_lines in

    handle_workflow workflows ["x",(0,4001);"m",(0,4001);"a",(0,4001);"s",(0,4001)] "in",
    entries
  ;;

  let cnt ((list, _): interval * 'a) = List.fold_left (fun acc (_,(x,x')) -> acc * (x'-x-1)) 1 list;;
  
  let is_valid (entry: entry) (interval: interval)  = 
    List.fold_left (fun acc (c,(x,x')) -> 
      if not acc then acc 
      else 
        let value = List.assoc c entry in
        value > x && value < x'
    ) true interval
  ;;

  let lookup intervals entry =
    let _, state = List.find (fun (i,_) -> is_valid entry i) intervals in state
  ;;

  let solve1 (intervals,entries) = 
    let accepted = List.filter (fun entry -> lookup intervals entry = Accept) entries in
    let sum = List.fold_left (fun acc entry ->
      List.fold_left (fun acc (_,value) -> acc+value) acc entry   
    ) 0 accepted in

    [ string_of_int sum ]
  ;;

  let solve2 (intervals,_) = 
    let accepted = List.filter (fun (_,state) -> state = Accept) intervals in
    let sum = List.fold_left (fun acc i -> acc + cnt i) 0 accepted in 

    [ string_of_int sum ]
  ;;
end