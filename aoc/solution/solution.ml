module type GenericSolutionIO = sig 
  type p_in
  type p1_out
  type p2_out

  (* parsiranje iz inputa *)
  val parse : in_channel -> p_in

  (* pisanje rešitve v output *)
  val write1 : out_channel -> p1_out -> unit
  val write2 : out_channel -> p2_out -> unit
end;;

module type GenericSolution = sig
  type p_in
  type p1_out
  type p2_out

  include GenericSolutionIO with type p_in := p_in and type p1_out := p1_out and type p2_out := p2_out
  val solve1 : p_in -> p1_out
  val solve2 : p_in -> p2_out
end;;

(* Implementacija za običajno branje nizov *)

let read_lines in_channel = 
  let rec aux acc = 
    match In_channel.input_line in_channel with 
    | None -> List.rev acc
    | Some s -> aux (s::acc) in
  aux []
;;

let write_solution out_channel solution = 
  let rec aux solution = 
    match solution with 
    | [] -> ()
    | h::t -> 
      Out_channel.output_string out_channel (h^"\n");
      Out_channel.flush out_channel;
      aux t in
  aux solution
;;

(* Pogosto uporabljene funkcije *)

let split_empty lines = 
  let p1, p2, _ = List.fold_left (fun (first,second,is_second) line -> 
    if line = "" then (first, second, true)
    else 
      if is_second then first, (line::second), is_second
      else (line::first), second, is_second
  ) ([],[], false) lines in

  List.rev p1, List.rev p2
;;

let bin_search v arr = 
  let rec aux i j = 
    if i > j then failwith "not found"
    else 
      let mid = (i+j)/2 in 
      if arr.(mid) = v then mid
      else
        if arr.(mid) < v then aux (mid+1) j
        else aux i (mid-1) in
  aux 0 (Array.length arr - 1)
;;

(* polimorfizirana dijkstra iz dneva 17 *)
let dijkstra (type state) start weight_func adj_func (module PQueue: Set.S with type elt = int*state) = 
  let dist = Hashtbl.create 10000 in
  let get_dist s = 
    match Hashtbl.find_opt dist s with
    | None -> max_int/2
    | Some v -> v 
  in
  let rec aux q = 
    if PQueue.is_empty q then dist
    else 
      let (d, point) = PQueue.min_elt q in
      let q = PQueue.remove (d,point) q in
      let q = List.fold_left (fun q state ->
        let d' = d + weight_func state in
        if d' < get_dist state then
          begin Hashtbl.add dist state d'; PQueue.add (d',state) q end
        else q
      ) q (adj_func point) in aux q in
  
  let state = start in
  let q = PQueue.add (0, state) PQueue.empty in
  aux q
;;

let transpose list =
  let rec aux list acc = 
    if list = [] then acc
    else 
      let col, rem = 
      List.fold_right(fun row (acc,rem) ->
        match row with
        | [] -> acc,[]
        | h::t -> (h::acc), (t::rem)
      ) list ([],[])  in
      aux rem (col::acc) in
  List.rev @@ List.tl @@ aux list []
;;

module StringSolutionIO : 
  (GenericSolutionIO with type p_in := string list and type p1_out := string list and type p2_out := string list) = struct

(*
  tukaj bi rad ohranil te deklaracije tipov, a tega ne morem ker mi da unused type warning, zato raje napišem še en pomožni modul 

  type p1_in = string list
  type p2_in = string list
  type p1_out = string list
  type p2_out = string list
 *)
  let parse = read_lines

  let write1 = write_solution
  let write2 = write_solution
end;;

module StringSolution = struct 
  type p_in = string list
  type p1_out = string list
  type p2_out = string list

  include StringSolutionIO 
end