open Props
open Bool
    
type opt =
  { verbose: bool ;
    dummy: bool }

let opts = ref { verbose = false ; dummy = false }

let set_opts ?verbose () =
  (match verbose with Some v -> opts := { !opts with verbose = v } | None -> ()) ;
  ()

let vv s k = if !opts.verbose then s ^ "(" ^ k ^ ")" else k

let item2s = function
  | Event st -> vv "ev" st
  | State st -> vv "st" st

let time_int2s = function
  | Time (a,b) -> Printf.sprintf "[%d,%d]" a b

let path2s (Path pa) = Common.sep item2s "." pa

let tcons2s = function
  | Notc -> ""
  | After it -> "after " ^ item2s it
  | Before it -> "before " ^ item2s it
  | Between (it,it2) -> Printf.sprintf "between %s and %s" (item2s it) (item2s it2)
  | After_until (it,it2) -> Printf.sprintf "after %s until %s" (item2s it) (item2s it2)
  | Within ti -> "within " ^ time_int2s ti
  | Lasting x -> Printf.sprintf "lasting %d" x

let rec obs2s = function
  | Obexpr ob -> bool2s obs2s ob
  | Enter ob -> "enter " ^ obs2s ob
  | Leave ob -> "leave " ^ obs2s ob
  | Atom (pa,it) -> path2s pa ^ " / " ^ item2s it

let pat2s = function
  | Present ob -> "present " ^ obs2s ob
  | Absent ob -> "absent " ^ obs2s ob
  | Leadsto (ob,ob2) -> Printf.sprintf "%s leadsto %s" (obs2s ob) (obs2s ob2)
  | Precedes (ob,ob2) -> Printf.sprintf "%s precedes %s" (obs2s ob) (obs2s ob2)
  | Always ob -> "always " ^ obs2s ob

let rec pdef2s = function
  | Pattern (pa,tc) -> pat2s pa ^ " " ^ tcons2s tc
  | Bool pd2 -> bool2s pdef2s pd2

let prop2s pro = Printf.sprintf "%s :: %s" (pro.p_name) (pdef2s pro.p_def)
    
