module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  module II =     struct type t = int * int           let compare = compare end
  module IIMap = Map.Make(II)

  type brick = { id: int; x: (int*int); y: (int*int); z: (int*int); on: int list }
  type p_in = brick list * int list

  let parse_line id line = 
    match String.split_on_char '~' line with 
    | [s;e] ->
      let split = (fun x -> List.map (int_of_string) (String.split_on_char ',' x)) in
      begin match List.combine (split s) (split e) with
        | [x;y;z] -> {id;x;y;z;on=[]}
        | _ -> failwith "invalid input"
      end 
    | _ -> failwith "invalid input"
  ;;

  let get map p = 
    match IIMap.find_opt p map with 
    | None -> 0,-1 | Some (v,id) -> v,id
  ;;

  let rec max_z map (di,dj) (i,j) n acc = 
    if n = 0 then acc
    else 
      let z,_ = get map (i,j) in
      max_z map (di,dj) (i+di,j+dj) (n-1) (max acc z)
  ;;

  let rec update_map (di,dj) z_min z_max id (i,j) n map set = 
    if n = 0 then map, List.sort_uniq (compare) set
    else 
      let z',id' = get map (i,j) in
      let set' = if z' = (z_min-1) then (id'::set) else set in
      let map' = IIMap.add (i,j) (z_max,id) map in  

      update_map (di,dj) z_min z_max id (i+di,j+dj) (n-1) map' set'
  ;;

  let fall (map,acc) brick= 
    let (x,x'), (y,y'), (z,z') = brick.x, brick.y, brick.z in
    let dir,n = if x=x' then (0,1),(y'-y+1) else (1,0),(x'-x+1) in

    let diff_z = -1 + z - max_z map dir (x,y) n 0 in
    let (z,z') = (z-diff_z, z'-diff_z) in

    let map',on = update_map dir z z' brick.id (x,y) n map [] in
    
    map',{id=brick.id; x=(x,x'); y=(y,y'); z=(z,z'); on}::acc
  ;;

  let fall_all bricks =
    let _,res = List.fold_left (fall) (IIMap.empty, []) bricks in res
  ;;

  let find_dangerous bricks =
    let dangerous = List.fold_left (fun acc brick -> 
      match brick.on with
      | [id] -> id::acc
      | _ -> acc
    ) [] bricks in
    List.filter ((<>) (-1)) @@ List.sort_uniq (compare) dangerous
  ;;

  let remove_support (bricks,to_remove) = 
    let tmp = List.map (fun brick -> 
      let on = List.filter (fun v -> not @@ List.mem v to_remove) brick.on in
      {brick with on}
    ) bricks in

    List.fold_left (fun (rem,fall) brick -> 
      if List.mem brick.id to_remove then (rem,fall)
      else 
        if List.length brick.on = 0 then (rem, brick.id::fall) 
        else (brick::rem, fall)
    ) ([], []) tmp
  ;;

  let rec cnt_removed state acc =
    match state with
    | _,[] -> acc
    | state ->
      let (rem, fall) = remove_support state in
      cnt_removed (rem,fall) (List.length fall + acc)
  ;;

  let parse in_channel = 
    let fell_bricks = in_channel  |> Solution.read_lines |> List.mapi (parse_line) 
                                  |> List.sort (fun b b'-> compare b.z b'.z) |> fall_all in

    fell_bricks, find_dangerous fell_bricks
  ;;

  let solve1 (fell_bricks,dangerous) = [string_of_int (List.length fell_bricks - List.length dangerous)];;
  let solve2 (fell_bricks,dangerous) = (* dovolj hitro kar odstraniti vse naivno *)
    let res = List.fold_left (fun acc id -> cnt_removed (fell_bricks,[id]) acc) 0 dangerous in

    [ string_of_int res ]
  ;;

end