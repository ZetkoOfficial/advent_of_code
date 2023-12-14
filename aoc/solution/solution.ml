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