open Props

(* Excessively verbose: show all the structure. *)
val set_opts: ?verbose:bool -> unit -> unit


val prop2s : property -> string
val pdef2s : pdef -> string
val pat2s : pat -> string
val obs2s : obs -> string
val tcons2s : tcons -> string
val path2s : path -> string
val time_int2s : time_int -> string
val item2s : item -> string
