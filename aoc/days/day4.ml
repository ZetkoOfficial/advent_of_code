module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution 

  type card = {_id: int; nums: int list; win: int list}
  type p_in = card list

  let parse_line line = 
    let parts = Str.split (Str.regexp ": \\| | ") line |> List.map (Str.split (Str.regexp " +")) in
    match parts with
    | [[_;id]; nums; win] -> { 
      _id =    int_of_string id; 
      nums =  List.map int_of_string nums; 
      win =   List.map int_of_string win
    } 
    | _ -> failwith "too many"  
  ;;

  let parse in_channel = 
    in_channel |> Solution.read_lines |> List.map parse_line
  ;;

  let count_matches game = 
    List.length @@ List.filter (fun n -> List.mem n game.win) game.nums
  ;;

  let to_score game = 
    truncate @@ 2.0 ** (float_of_int @@ count_matches game - 1)
  ;;

  let rec update_arr games count_arr i =
    match games with
    | [] -> ()
    | game::t ->
      let (matches, per) = (count_matches game, count_arr.(i)) in
      for i=(i+1) to i+matches do
        count_arr.(i) <- count_arr.(i) + per
      done;

      update_arr t count_arr (i+1)
  ;;

  let solve1 input = 
    let scores = List.map to_score input in

    [ string_of_int @@ List.fold_left (+) 0 scores ]
  ;;

  let solve2 input =
    let count_arr = Array.make (List.length input) 1 in
    update_arr input count_arr 0;

    [ string_of_int @@ Array.fold_left (+) 0 count_arr ]
  ;;
end