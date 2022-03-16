(* Read aut files *)

type path = string

type label = string

type arc = int * label * int

type state = string

type aut =
  { nb_places: int ;

    (* Each place may have a state. *)
    states: (int * state) list ;
    
    trans: arc list }

val read_aut: path -> aut

(* Builds a aut structure given only the arc list and states. *)
val mk_aut: (int * state) list -> arc list -> aut

(* TODO : remove dead places ? *)

type format = AUT | AUT_SP2

val write_aut: format -> path -> aut -> unit
  
