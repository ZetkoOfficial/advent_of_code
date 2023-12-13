module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type p_in = (int list * char list) list

  let parse_line line = 
    match Str.split (Str.regexp " +") line with 
    | [p1;p2] -> 
      List.map (int_of_string) (Str.split (Str.regexp ",") p2),
      p1 |> String.to_seq |> List.of_seq
    | _ -> failwith "error parse"
  ;;

  let parse in_channel = 
    in_channel |> Solution.read_lines |> List.map parse_line
  ;;  

  let check_rem_empty line = List.for_all ((<>)'#') line ;;
  let rec check_rem_full line k = 
    match line,k with
    | rem,0 -> true, rem
    | [],_ -> false, []
    | '.'::_,_ -> false, []
    | _::t,_ -> check_rem_full t (k-1)
  ;;

  let check_fill line k = 
    let ok,rem = check_rem_full line k in
    match ok, rem with
    | false, _ -> false, [] 
    | true, [] -> true, []
    | true, h::rem ->
      if h <> '#' then true, rem
      else false, []
  ;;   

  let possible rules line = (* če faila smo šli predaleč *)
    let mem = Array.make_matrix (10+List.length rules) (10+List.length line) None in
    let rec aux i j rules line = 
      match mem.(i).(j) with
      | None -> 
        let value = match rules, line with
        | [], _ -> if check_rem_empty line then 1 else 0
        | _, [] -> 0
        | hr::tr, hl::tl ->
          let leave = aux i (j+1) (hr::tr) tl in
          if hl = '.' then leave
          else
            let take = begin
              let ok, rem = check_fill tl (hr-1) in
              if not ok then 0
              else aux (i+1) (j+hr+1) tr rem
            end in

            if hl = '#' then take
            else leave + take in

          mem.(i).(j) <- Some value;
          value
      | Some value -> value in
    
    aux 0 0 rules line
  ;;

  let rec copy list n extra = 
    if n = 1 then list 
    else list @ extra @ copy list (n-1) extra
  ;;

  let solve1 rows = [ 
    string_of_int @@ List.fold_left (fun acc (rules, line) -> acc + possible rules line) 0 rows 
  ];;

  let solve2 rows = [ 
    string_of_int @@ List.fold_left (fun acc (rules, line) -> 
      acc + possible (copy rules 5 []) (copy line 5 ['?'])
    ) 0 rows
  ];;
    
end