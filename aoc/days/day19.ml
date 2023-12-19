module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type transition = Next of string | Skip | Accept | Reject
  type condition = string * (int -> transition)
  type entry = (string * int) list 

  module StringMap = Map.Make(String)

  type p_in = condition list StringMap.t * entry list

  let split_empty lines = 
    let p1, p2, _ = List.fold_left (fun (first,second,is_second) line -> 
      if line = "" then (first, second, true)
      else 
        if is_second then first, (line::second), is_second
        else (line::first), second, is_second
    ) ([],[], false) lines in

    List.rev p1, List.rev p2
  ;;

  let parse_conditions conditions: condition list = 
    List.fold_left (fun acc condition_str -> 
      let condition_str = Str.global_replace (Str.regexp "<") " < " condition_str in
      let condition_str = Str.global_replace (Str.regexp ">") " > " condition_str in

      let cond = match Str.split (Str.regexp " \\|:") condition_str with
      | [cat;"<";value;"A"] -> cat, (fun x -> if x < int_of_string value then Accept else Skip) 
      | [cat;"<";value;"R"] -> cat, (fun x -> if x < int_of_string value then Reject else Skip) 
      | [cat;">";value;"A"] -> cat, (fun x -> if x > int_of_string value then Accept else Skip)  
      | [cat;">";value;"R"] -> cat, (fun x -> if x > int_of_string value then Reject else Skip)  
      | [cat;"<";value;workflow] -> cat, (fun x -> if x < int_of_string value then Next workflow else Skip)  
      | [cat;">";value;workflow] -> cat, (fun x -> if x > int_of_string value then Next workflow else Skip)
      | ["A"] -> "", (fun _ -> Accept)
      | ["R"] -> "", (fun _ -> Reject)
      | [workflow] -> "", (fun _ -> Next workflow)
      | _ -> failwith "unmached input" in cond::acc
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
    

  let parse in_channel = 
    let workflow_lines, entry_lines = in_channel |> Solution.read_lines |> split_empty in
    List.fold_left (parse_workflow) StringMap.empty workflow_lines, 
    List.fold_left (parse_entry) [] entry_lines
  ;;

  let determine_condition (symb, func) entry = 
    if symb = "" then func 0
    else 
      if List.mem_assoc symb entry then
        func @@ List.assoc symb entry
      else Skip
  ;;


  let rec determine workflows entry workflow =
    let wf = StringMap.find workflow workflows in
    let res = List.fold_left (fun acc con -> 
      if acc = Skip then determine_condition con entry
      else acc
    ) Skip wf in
    match res with
    | Skip -> failwith "invalid"
    | Next workflow' -> determine workflows entry workflow'
    | other -> other
  ;;

  let solve1 (workflows,entries) = 
    let accepted = List.filter (fun entry -> determine workflows entry "in" = Accept) entries in
    let sum = List.fold_left (fun acc entry ->
      List.fold_left (fun acc (_,value) -> acc+value) acc entry   
    ) 0 accepted in

    [ string_of_int sum ]
  ;;

  (* TODO: treba je z intervali določati območja ki dajo isto vrednost, 
     a bom za to rabil znova parsirati input na drugačen način *)
  let solve2 _ = [""];;

end