module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution 
  type p_in = ((char list) * int) list

  let parse in_channel = 
    let lines = in_channel |> Solution.read_lines in 
    
    List.map (fun line -> 
      match Str.split (Str.regexp " +") line with
      | [hand;bid] -> hand |> String.to_seq |> List.of_seq, int_of_string bid
      | _ -> failwith "parse error" 
    ) lines
  ;;

  module CMap = Map.Make(Char)
  
  let incr map c = 
    match CMap.find_opt c map with
    | None ->     CMap.add c 1        map
    | Some num -> CMap.add c (1+num)  map
  ;;

  let to_occ_map hand = List.fold_left incr CMap.empty hand;;
  
  let count_n_same occ n = 
    List.length @@ List.filter (fun (_,num) -> n = num) (CMap.bindings occ)
  ;;

  let card_list = ['A',1;'K',2;'Q',3;'J',4;'T',5;'9',6;'8',7;'7',8;'6',9;'5',10;'4',11;'3',12;'2',13];;
  let card_str c is_part2 = 
    if c = 'J' && is_part2 then 14
    else List.assoc c card_list
  ;;

  let hand_to_val hand = 
    let occ = to_occ_map hand in
    let same = (
      count_n_same occ 5, count_n_same occ 4,
      count_n_same occ 3, count_n_same occ 2
    ) in

    match same with
    | (1,0,0,0) -> 1 | (0,1,0,0) -> 2 | (0,0,1,1) -> 3 
    | (0,0,1,0) -> 4 | (0,0,0,2) -> 5 | (0,0,0,1) -> 6
    | (0,0,0,0) -> 7
    | _ -> failwith "invalid cards"
  ;;

  let best_hand_val hand = (* lahko tudi kar najdemo najpogostejšo *)
    let cmn,_ = List.fold_left (fun (c,occ) (c',occ') -> 
      if occ' >= occ && c' != 'J' then (c',occ') else (c, occ) 
    ) ('A',0) (CMap.bindings @@ to_occ_map hand) in
    
    hand_to_val (List.map (fun c -> if c = 'J' then cmn else c) hand)
  ;;

  let compare_hands (hand1,val1) (hand2,val2) is_part2 =
    let c1 = compare val1 val2 in

    if c1 <> 0 then c1
    else List.compare (fun a b -> compare (card_str a is_part2) (card_str b is_part2)) hand1 hand2
  ;;

  let solve with_values is_part2 = 
    let sorted = List.rev @@ List.sort (fun (h1,_) (h2,_) -> compare_hands h1 h2 is_part2) with_values in
    let winnings = List.mapi (fun i (_,bid) -> (i+1)*bid) sorted in

    [ string_of_int @@ List.fold_left (+) 0 winnings ]
  ;;

  let solve1 hands = 
    let with_values = List.map (fun (hand,bid) -> ((hand, hand_to_val hand), bid)) hands in
    solve with_values false
  ;;

  let solve2 hands =
    let with_values = List.map (fun (hand,bid) -> ((hand, best_hand_val hand), bid)) hands in
    solve with_values true
  ;;

end