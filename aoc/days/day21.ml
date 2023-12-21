module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  module II =     struct type t = int * int           let compare = compare end

  module IIMap = Map.Make(II)
  module IISet = Set.Make(II)

  type grid_elem = Plot | Rock
  type p_in = (grid_elem IIMap.t * int * int) * (int*int)

  let enum list =
    let _,(start,res) = List.fold_left (fun (i,(start,acc)) line -> 
      let _,(start,acc) = String.fold_left (fun (j,(start,acc)) char -> 
        let acc' = match char with
        | '.' -> start,     IIMap.add (i,j) Plot acc
        | '#' -> start,     IIMap.add (i,j) Rock acc
        | 'S' -> Some (i,j),IIMap.add (i,j) Plot acc
        | _ -> failwith "invalid input" in
        (j+1), acc'
      ) (0,(start,acc)) line in (i+1),(start,acc) 
    ) (0,(None,IIMap.empty)) list in
    match start with
    | Some s -> (res,(List.length list), String.length (List.hd list)), s
    | None -> failwith "missing start"
  ;;

  let parse in_channel = in_channel |> Solution.read_lines |> enum;;

  let get_adj map (i,j) =
    let adj = [(i+1,j);(i-1,j);(i,j+1);(i,j-1)] in
    List.filter (fun p -> IIMap.find_opt p map = Some Plot) adj
  ;;

  let rec n_steps map adj_func n cur = 
    if n = 0 then List.sort_uniq (compare) cur
    else
    let cur' = List.sort_uniq (compare) @@ List.fold_left (fun acc p -> 
      (adj_func p) @ acc  
    ) [] cur in
    n_steps map adj_func (n-1) cur'
  ;; 

  let solve1 ((map,_,_),start) = 
    let possible = n_steps map (get_adj map) 64 [start] in
    [ string_of_int @@ List.length possible ]
  ;;

  let to_normal v n = ((v mod n) + n) mod n;;
  let get_adj_loop map (n,m) (i,j) =
    let adj = [(i+1,j);(i-1,j);(i,j+1);(i,j-1)] in
    List.filter (fun (i,j) -> IIMap.find_opt (to_normal i n, to_normal j m) map = Some Plot) adj
  ;;

  (* 
    naš input je sestavljen iz diamanta in delov izven diamanta,
    deli izven diamanta pa skupaj zgradijo diamant identičen začetnemu
    
    potem so mesta ki jih dosežemo s premikom za višino grida + x natanko tista
    ki jih dosežemo z x, le da nekatere štejemo večkrat (zaradi simetrije bo premik za x+l vedno znotraj diamanta,
    in ker sta centra oddajena za natanko l bodo te točke enake)

    opazujmo torej kako se spreminja število točk pri x, x+l, x+2l, ...
    l je v našem inputu 131
  *)

  (*  
    ta komentrana koda je izračunala prvih 500 dolžin ki jih potem na roke preverimo kaj se dogaja s tistimi zaporedji

     let rec nums (map,n,m) k state acc = 
      if k = 0 then List.rev acc
      else 
        let state' = n_steps map (get_adj_loop map (n,m)) 1 state in
        nums (map,n,m) (k-1) state' ((List.length state')::acc)
    ;;

    let solve2 ((map,n,m),start) = 
      let ns = nums (map,n,m) (500) [start] [] in
      List.map (string_of_int) ns
    ;; 
  *)

  (* 
    primer dveh takih zaporeij sta
    
    a(1) = 4, a( 1 +131) = 15534, a(1 + 2*131) = 61252, a(1 + 3*131) = 137158
    a(2) = 8, a( 2 +131) = 15758, a(2 + 2*131) = 61696, a(2 + 3*131) = 137822

    opazimo da dA = 15530, 45718, 75906, ... -> d^2A = 30188, 30188, ...  in
            da dB = 15750, 45938, 76126, ... -> d^2B = 30188, 30188, ...

    zgleda torej da so druge razlike konstantne in neodovisne od začetnga x in enake 30188
    30188 pa je tudi enako dvokratniku doseglivih v gridu? (131^2(vseh) - 2061(kamnov) - 6(zaprtih) )
    -> numerično preverjeno (verjetno je v teoriji povezano z tem da se parnosti ohranjajo pri prehodih za l
      (prazen stolpec in vrstica) in simetrijo ter dejstvom da se število dostopnih središč veča z zaporedjem 4,8,12,16...
      v vsakem koraku se pa nesporedno prejšna dostopna povečajo iz 1/4 na 1/2 tako je zaporeje deležev celotne plošče 8+4, 12+8, 16+12 = 12,20,28
      vsakič povečano za 8 četrnin polja (če predsptavimo da je sodih in lihih dostopnih enako potem) je to natanko število vseh dostopnih v gridu * 2, 
      a bi rabil to še malce bolj podrobno premisliti)
    
    tako potem velja dA(n) = dA(1) + 30188(n-1)
    in a(x+131(n-1)) = a(x) + vsota od 1 do n-1 od dA(1) + 30188(n-1) = a(x) + (n-1)dA(1) + 15094*(n^2-3n+2)
  *)

  (* 
    ker 26501365 = 20239 * l + 56 potrebujemo izračunati a(56+131n), kjer n = 20239 + 1
  *)

  let rec reachable map start all =
    let start' = IISet.fold (fun p acc -> IISet.add_seq (List.to_seq (get_adj map p)) acc) start IISet.empty in
    let start' = IISet.filter (fun p -> not (IISet.mem p all)) start' in

    let all' = IISet.union all start' in

    if IISet.cardinal start' = 0 then IISet.cardinal all
    else reachable map start' all'
  ;;

  let solve2 ((map,n,m),start) =
    let steps = 26501365 in
    let rem = steps mod n in
    let reachable = reachable map (IISet.add start IISet.empty) (IISet.add start IISet.empty) in

    let ax_state = n_steps map  (get_adj_loop map (n,m)) rem [start] in
    let ax_state' = n_steps map (get_adj_loop map (n,m)) 131 ax_state in
    
    let ax = List.length ax_state in
    let dA = List.length ax_state' - ax in

    let n = steps/n + 1 in
    let res = ax + (n-1)*dA + reachable * (n*n-3*n+2) in

    [ string_of_int res ]
  ;;

end