module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution 
  
  type rgb = { r: int; g: int; b: int }
  type game = { id: int; c: rgb list }
  type p_in = game list

  let lines_to_parts input = List.map (fun vrstica -> Str.split (Str.regexp ":\\|;") vrstica) input;;

  let part_to_rgb part = 
    let rec aux split p acc = 
      match split with
      | [] -> acc
      | "red"::t ->   aux t p {acc with r = acc.r + int_of_string p}
      | "green"::t -> aux t p {acc with g = acc.g + int_of_string p}
      | "blue"::t ->  aux t p {acc with b = acc.b + int_of_string p}
      | h::t -> aux t h acc in
      aux (Str.split (Str.regexp " \\|,") part) "" {r=0;g=0;b=0}
  ;;

  let to_games input =
    let to_game parts =
      let (hd, rest) = (List.hd parts, List.tl parts) in 
      let c = List.map (fun part -> part_to_rgb part) rest in
      { id=int_of_string @@ List.hd @@ List.tl (Str.split (Str.regexp " ") hd); c} in
      
    List.map (fun game -> to_game game) input
  ;;

  let parse in_channel = 
    in_channel |> Solution.read_lines |> lines_to_parts |> to_games;; 
  ;;

  let game_max game = 
    List.fold_left (fun acc c -> 
      {r = max acc.r c.r; g = max acc.g c.g; b = max acc.b c.b}
    ) {r=0;g=0;b=0} (game.c)
  ;;

  let solve1 input =
    let valid = List.filter (fun game -> 
      let gt = game_max game in gt.r <= 12 && gt.g <= 13 && gt.b <= 14
    ) input in
    
    [ string_of_int @@ List.fold_left (fun acc g -> acc + g.id) 0 valid ]
  ;;

  let solve2 input = 
    let min = List.map (fun game -> 
      let gt = game_max game in gt.r * gt.g * gt.b
    ) input in 
    
    [ string_of_int @@ List.fold_left (+) 0 min ]
end