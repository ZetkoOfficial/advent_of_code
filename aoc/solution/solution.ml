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
      aux t in
  aux solution
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