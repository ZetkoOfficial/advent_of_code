module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type hashmap = {
    array: (string*int) list array (* shranjeno v obratnem redu *)
  }

  let hash str = 
    String.fold_left (fun acc char ->
      ((acc + Char.code char) * 17) mod 256   
    ) 0 str
  ;;

  let create_hashmap () = {array=Array.make 256 []};;
  
  let remove key map =
    let hashed = hash key in 
    let list = map.array.(hashed) in
    map.array.(hashed) <- List.remove_assoc key list
  ;;

  let insert key value map = 
    let hashed = hash key in
    let list = map.array.(hashed) in
    if List.mem_assoc key list then
      map.array.(hashed) <- List.map (fun (key', lense) -> if key' = key then (key', value) else (key', lense)) list 
    else
      map.array.(hashed) <- (key,value)::list
  ;;

  let focus_power map =
    let _,res = Array.fold_left (fun (i,acc) list ->
      let _, acc' = List.fold_left (fun (j,acc) (_,value) -> 
        j+1, acc + j*value
      ) (1, 0) (List.rev list) in
      i+1, acc + i*acc' 
    ) (1,0) map.array in res
  ;;

  let parse in_channel = 
    in_channel |> Solution.read_lines |> List.hd |> 
    String.split_on_char ',' 
  ;;

  let instruction line = Str.split (Str.regexp "=\\|-") line;;

  let solve1 input = 
    let hashed = List.map (hash) input in
    
    [ string_of_int @@ List.fold_left (+) 0 hashed ]
  ;;

  let solve2 input = 
    let map = create_hashmap () in
    List.iter (fun parts -> 
      match parts with
      | [key_remove] -> remove key_remove map
      | [key_insert;value] -> insert key_insert (int_of_string value) map
      | list -> print_int @@ List.length list; failwith "invalid instruction" 
    ) (List.map (instruction) input);

    [string_of_int @@ focus_power map]
  ;;
end