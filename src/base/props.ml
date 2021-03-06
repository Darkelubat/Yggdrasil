open Bool

(* State or Event *)
type item = | State of string
            | Event of string

(* Timing interval *)
type time_int = Time of int * int

(* Path to follow *)
type path = Path of item list

(* Timing constraint *)
type tcons = | Notc
             | After of item
             | Before of item
             | Between of item * item
             | After_until of item * item 
             | Within of time_int
             | Lasting of int

(* Observation as path on event or state *)
type obs = | Obexpr of obs bexpr
           | Enter of obs
           | Leave of obs
           | Atom of path * item

(* Observateur d'item en fiacre devrait avoir une interface commune sur des variables ou des signaux a cause de la différence entre state et event *)

(* Pattern for property *)
type pat = | Present of obs
           | Absent of obs
           | Leadsto of obs * obs
           | Precedes of obs * obs
           | Always of obs

(* Pattern or boolean expression *)
type pdef = | Pattern of pat * tcons
            | Bool of pdef bexpr

(* The final property *)
type property = { p_name: string ;
                  p_def: pdef}

