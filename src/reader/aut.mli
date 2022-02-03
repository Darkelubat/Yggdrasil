(* Read aut files *)

type path = string

type label = string

type arc = int * label * int

type aut =
  { nb_places: int ;
    nb_trans: int ;
    trans: arc list }

val read_aut: path -> aut

(* Builds a aut structure given only the arc list *)
val mk_aut: arc list -> aut

(* TODO : remove dead places ? *)

val write_aut: path -> aut -> unit
  
