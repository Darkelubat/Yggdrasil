open Bool
(* State or Event *)
type item = | State of string
            | Event of string;;

(* Timing interval *)
type time_int = Time of int*int;;

(* Path to follow *)
type path = Path of item list;;

(* Timing constraint *)
type tcons = | Notc
             | After of item
             | Before of item
             | Between of item*item
             | After_until of item*item 
             | Within of time_int
             | Lasting of int;;

(* Observation as path on event or state *)
type obs = | Obexpr of obs bexpr
           | Enter of obs
           | Leave of obs
           | Atom of path*item;;

(* Pattern for property *)
type pat = | Present of obs
           | Absent of obs
           | Leadsto of obs*obs
           | Precedes of obs*obs
           | Always of obs;;

(* Pattern or boollean expression *)
type pdef = | Pattern of pat*tcons
            | Bool of pdef bexpr;;

(* The final property *)
type property = {p_name:string;
                 p_def:pdef};;

let rec list_last = function
   | [] -> failwith "list_last"
   | [x] -> x
   | _ :: xs -> list_last xs
(* Builds a string from a list of items.
  * last_sep: last separator
  * max: max elements printed. If there are more, put an ellipsis ... *)
let sep map sp ?(max = max_int) ?(last_sep=sp) l =
   let rec loop count acu = function
     | [] -> ""
     | [x] ->
       (* Singleton *)
       if acu = "" then map x
       else
         (* Last element *)
         acu ^ last_sep ^ (map x)

     | x :: xs ->
       if count >= max then acu ^ sp ^ ".." ^ sp ^ (map (list_last xs))
       else loop (count+1) (if acu = "" then map x else acu ^ sp ^ (map x)) xs
   in
   loop 1 "" l

(*Convert to String*)

let item2s i = match i with
| Event st -> st
| State st -> st;;

let rec path2s pa = match pa with
|Path x -> begin match x with
  |x::rest -> (item2s x)^(path2s rest)
  |[] -> "\n"end;;

let rec obs2s ob1 = match ob1 with
| Obexpr ob -> (bool2s ob)
           | Enter ob -> obs2s ob
           | Leave ob -> obs2s ob
           | Atom (pa,it) -> (path2s pa)^(item2s it);;

let pat2s p = match p with
| Present ob -> obs2s ob
           | Absent ob -> obs2s ob
           | Leadsto (ob,ob2) -> (obs2s ob)^(obs2s ob2)
           | Precedes (ob,ob2) -> (obs2s ob)^(obs2s ob2)
           | Always ob -> obs2s ob;;

let tcons2s tc = match tc with
| Notc -> "[O,inf]"
| After it -> item2s it
             | Before it -> item2s it
             | Between (it,it2) -> Printf.sprintf "%s,%s\n" (item2s it) (item2s it2)
             | After_until (it,it2) -> Printf.sprintf "%s,%s\n" (item2s it) (item2s it2)
             | Within ti -> time_int2s ti
             | Lasting it -> item2s it;;

let time_int2s ti = match ti with
| Time (a,b) -> Printf.sprintf "[%x,%x]\n" a b;;

let rec pdef2s pd = match pd with
|Pattern (pa,tc) -> (pat2s pa)^(tcons2s tc)
|Bool pd2 -> (bool2s pd2);;

let prop2s pro = (pro.p_name)^(pdef2s pro.p_def);;