module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type direction = Left | Right | Up | Down

  type p_in = ((direction * int) list) * ((direction * int) list)

  let char_to_int char = 
    if char >= '0' && char <= '9' then Char.code char- Char.code '0'
    else Char.code char - Char.code 'a' + 10
  ;;

  let hex_to_int hex = String.fold_left (fun acc c -> 16*acc + char_to_int c) 0 hex;;

  let hex_to_dir hex = 
    let hex = Str.global_replace (Str.regexp "#\\|(\\|)") "" hex in
    let amt = hex_to_int @@ Str.first_chars hex 5 in 
    let last = Str.last_chars hex 1 in

    match last with
    | "0" -> Right, amt | "1" -> Down, amt
    | "2" -> Left, amt  | "3" -> Up, amt

    | _ -> failwith "invalid hex"
  ;;

  let parse in_channel = 
    in_channel |> Solution.read_lines |> List.map (fun line ->
      match String.split_on_char ' ' line with
      | ["L";num;hex] -> (Left,int_of_string num), (hex_to_dir hex)
      | ["R";num;hex] -> (Right,int_of_string num), (hex_to_dir hex)
      | ["U";num;hex] -> (Up,int_of_string num), (hex_to_dir hex)
      | ["D";num;hex] -> (Down,int_of_string num), (hex_to_dir hex)
      | _ -> failwith "invalid input"
    ) |> List.split
  ;;

  let to_points ins = 
    let rem, points = List.fold_left (fun ((i,j),acc) ins -> 
      let (di,dj) = match ins with
      | Left,   n  -> (0,-n)
      | Right,  n  -> (0,n)
      | Up,     n  -> (-n, 0)
      | Down,   n  -> (n,0) in
      (di+i,dj+j), (i,j)::acc
    ) ((0,0),[]) ins in
    rem::points
  ;;

  let area_twice points =
    let rec aux points area = 
      match points with
      | [] | [_] -> area
      | (i1,j1)::(i2,j2)::t -> aux ((i2,j2)::t) (area + i1*j2-i2*j1) in
    abs (aux ( points @ [List.hd points] ) 0)
  ;;

  let count_boundry points = 
    let rec aux points b = 
      match points with
      | [] | [_] -> b
      | (i1,j1)::(i2,j2)::t -> aux ((i2,j2)::t) (b + abs (i1-i2) + abs (j1-j2)) in
    aux ( points @ [List.hd points] ) 0
  ;;

  let total_filled points = (* Pick-ov izrek *)
    let b =  count_boundry points in
    let area_t = area_twice points in
    let i = (area_t - b + 2)/2 in

    b+i
  ;;

  let solve1 (ins1,_) = 
    let points = to_points ins1 in
    [ string_of_int @@ total_filled points ]
  ;;
  
  let solve2 (_,ins2) = 
    let points = to_points ins2 in
    [ string_of_int @@ total_filled points ]
  ;;
end