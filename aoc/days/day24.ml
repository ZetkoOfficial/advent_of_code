module  Resitev : Solution.GenericSolution = struct 
  include Solution.StringSolution

  type[@warning "-69"] point = { x:int; y:int; z:int; vx:int; vy:int; vz:int }
  type p_in = point list

  let parse_line line: point = 
    match Str.split (Str.regexp " +@ +") line with
    | [pos;vel] ->
      let pos,vel = Str.split (Str.regexp ", +") pos, Str.split (Str.regexp ", +") vel in
      let pos,vel = List.map (int_of_string) pos, List.map (int_of_string) vel in
      
      begin match  pos, vel with
        | [x;y;z], [vx;vy;vz] -> {x;y;z;vx;vy;vz}
        | _ -> failwith "invalid input"
      end
    | _ -> failwith "invalid input"
  ;;

  let parse in_channel = in_channel |> Solution.read_lines |> List.map (parse_line);;
  let is_inside x y a b = x >= a && x <= b && y >= a && y <= b;;

  let check_intersection p p' a b=
    let det = -p.vx*p'.vy + p'.vx*p.vy in
    let dx,dy = p'.x - p.x, p'.y - p.y in

    if det = 0 then false (* ni rešitve *)
    else
      let det_t, det_t' = 
      -p'.vy * dx + p'.vx * dy,
      -p.vy * dx + p.vx * dy in

      let t,t' = float_of_int det_t /. float_of_int det, float_of_int det_t' /. float_of_int det in
      if t < 0.0 || t' < 0.0 then false
      else
        let x,y = float_of_int p.x +. t*. float_of_int p.vx, float_of_int p.y +. t *. float_of_int p.vy in
        is_inside x y a b
  ;;

  let solve1 points = 
    let a, b = 200000000000000., 400000000000000. in
    let times = List.mapi (fun i v -> i,v) points in

    let res = List.fold_left (fun acc (i,r) -> 
      List.fold_left (fun acc (i',r') ->
        if i >= i' then acc
        else 
          if check_intersection r r' a b then acc+1
          else acc
      ) acc times
    ) 0 times in

    [ string_of_int res ]
  ;;

  (* 
    Rešiti moramo sistem enačb (r-xi) x (v-vi) = 0, za x in v, kjer je r lega kamna, v hitrost kamna, 
    xi lege toče in vi hitrosti toče

    izberimo prve tri točke:
    291493672529314, 259618209733833, 379287136024123 @ -9, 119, -272
    308409248682955, 156803514643857, 424989308414284 @ -78, 236, -255
    195379943194796, 213851381371727, 355270583377422 @ 25, 14, -15

    in rešimo sistem teh treh enačb z Wolfram matematiko:

    eq[xi_, vi_] := Cross[{x1, x2, x3} - xi, {v1, v2, v3} - vi] == {0, 0, 0}
    Reduce[
      eq[{291493672529314, 259618209733833, 379287136024123}, {-9, 119, -272}] &&
      eq[{308409248682955, 156803514643857, 424989308414284}, {-78,236, -255}] &&
      eq[{195379943194796, 213851381371727, 355270583377422}, {25, 14, -15}],
      {x1, x2, x3, v1, v2, v3}
    ]

    kar da rezultat
    x1 == 370994826025810 && x2 == 410411158485339 && x3 == 167572107691063 && v1 == -193 && v2 == -230 && v3 == 218

    torej je vsota pozicije: 370994826025810+410411158485339+167572107691063 = 948978092202212
  *)
  let solve2 _ = ["948978092202212";"Za prevod problema v gaussovo eliminacijo(katero potem ni problem implementirati v poljubnem jeziku) pa si lahko ogledate day24.pdf"];;

end