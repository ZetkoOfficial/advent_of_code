module Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution
  
  type direction = Left | Right | Up | Down | Start
  type state = (int*int) * (direction * int)

  module II =     struct type t = int * int   let compare = compare end
  module State =  struct type t = state       let compare = compare end
  module IState = struct type t = int * state let compare = compare end
  
  module IISet = Set.Make(IState)
  module IIMap = Map.Make(II)
  module StateMap = Map.Make(State)

  type p_in = (int IIMap.t) * int * int

  let enum list =
    let _, res = List.fold_left (fun (i,acc) line -> 
      let _, acc = String.fold_left (fun (j,acc) char -> 
        (j+1), IIMap.add (i,j) (Char.code char - Char.code '0') acc
      ) (0,acc) line in (i+1),acc 
    ) (0,IIMap.empty) list in
    res, (List.length list), String.length (List.hd list)
  ;;

  let parse in_channel = in_channel |> Solution.read_lines |> enum;;

  let get_adj_1 (((i,j),(dir,num)): state) map: state list =
    let adj = match dir,num with
    | Start,_     -> [(i,j+1),(Right,1);(i+1,j),(Down,1);]

    | Right,3     -> [(i+1,j),(Down,1);(i-1,j),(Up,1)]
    | Left, 3     -> [(i+1,j),(Down,1);(i-1,j),(Up,1)]
    | Up,   3     -> [(i,j+1),(Right,1);(i,j-1),(Left,1)]
    | Down, 3     -> [(i,j+1),(Right,1);(i,j-1),(Left,1)]
    
    | Up,   n     -> [(i-1,j),(Up,n+1);(i,j+1),(Right,1);(i,j-1),(Left,1)]
    | Down, n     -> [(i+1,j),(Down,n+1);(i,j+1),(Right,1);(i,j-1),(Left,1)]
    | Right,n     -> [(i+1,j),(Down,1);(i-1,j),(Up,1);(i,j+1),(Right,n+1)]
    | Left, n     -> [(i+1,j),(Down,1);(i-1,j),(Up,1);(i,j-1),(Left,n+1)] in
    List.filter (fun (p,_) -> IIMap.mem p map) adj
  ;;

  let get_adj_2 (((i,j),(dir,num)): state) map: state list =
    let adj = match dir,num with
    | Start,_     -> [(i,j+1),(Right,1);(i+1,j),(Down,1);]

    | Right,10     -> [(i+1,j),(Down,1);(i-1,j),(Up,1)]
    | Left, 10     -> [(i+1,j),(Down,1);(i-1,j),(Up,1)]
    | Up,   10     -> [(i,j+1),(Right,1);(i,j-1),(Left,1)]
    | Down, 10     -> [(i,j+1),(Right,1);(i,j-1),(Left,1)]

    | Up,   n when n < 4  -> [(i-1,j),(Up,n+1)]
    | Down, n when n < 4  -> [(i+1,j),(Down,n+1)]
    | Right,n when n < 4  -> [(i,j+1),(Right,n+1)]
    | Left, n when n < 4  -> [(i,j-1),(Left,n+1)]
    
    | Up,   n     -> [(i-1,j),(Up,n+1);(i,j+1),(Right,1);(i,j-1),(Left,1)]
    | Down, n     -> [(i+1,j),(Down,n+1);(i,j+1),(Right,1);(i,j-1),(Left,1)]
    | Right,n     -> [(i+1,j),(Down,1);(i-1,j),(Up,1);(i,j+1),(Right,n+1)]
    | Left, n     -> [(i+1,j),(Down,1);(i-1,j),(Up,1);(i,j-1),(Left,n+1)] in
    List.filter (fun (p,_) -> IIMap.mem p map) adj
  ;;

  let get_dist (s: state) dist = 
    match StateMap.find_opt s dist with
    | None -> max_int/2
    | Some v -> v
  ;;

  let dijkstra start map adj_func = 
    let rec aux dist q = 
      if IISet.is_empty q then dist
      else 
        let (d, point) = IISet.min_elt q in
        let q = IISet.remove (d,point) q in
        let dist, q = List.fold_left (fun (dist,q) (p,dir) ->
          let d' = d + IIMap.find p map in
          if d' < get_dist (p,dir) dist then
            StateMap.add (p,dir) d' dist, IISet.add (d',(p,dir)) q
          else dist, q
        ) (dist,q) (adj_func point map) in aux dist q in
    
    let state = start, (Start,0) in
    let dist = StateMap.add state 0 StateMap.empty in
    let q = IISet.add (0, state) IISet.empty in
    aux dist q
  ;;

  let get_min_dist p dist min_steps=
    StateMap.fold (fun (p',(_,steps)) value acc -> if p=p' && steps >= min_steps then min acc value else acc) dist max_int
  ;; 

  let solve1 (map,n,m) = 
    let dist = dijkstra (0,0) map get_adj_1 in 
    [ string_of_int @@ get_min_dist (n-1,m-1) dist 0 ]
  ;; 

  let solve2 (map,n,m) = 
    let dist = dijkstra (0,0) map get_adj_2 in 
    [ string_of_int @@ get_min_dist (n-1,m-1) dist 4 ]
  ;; 
end