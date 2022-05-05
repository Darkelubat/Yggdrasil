open Base.Common

open Aut

(*

(*** Equivalence relation in a hashtable (ints only) ***)

type eq_v =
  (* Elements in the same class. *)
  | Set of int list

  (* Link to the class repr. *)
  | Link of int

(* By default, each class is a singleton. *)
let mk_rel () = Assoc.create ~size:200 ~init:(fun x -> Set [x]) ()

let rec get_repr tbl x =
  match Assoc.get tbl x with
  | Set l -> (x,l)
  | Link i -> get_repr tbl i

(* Add the relation a~b into the table. *)
let add_rel tbl a b =

  let (a,la) = get_repr tbl a
  and (b,lb) = get_repr tbl b in
  
  if a = b then () (* Already in the table *)
  else
    begin
      let linka = Link a in
      List.iter (fun bb -> Assoc.set tbl bb linka) lb ;
      Assoc.set tbl a (List.rev_append lb la)
    end

(*************************************************)

*)

module S = Set.Make (struct type t = int let compare = Stdlib.compare end)
module M = Map.Make (struct type t = S.t let compare = S.compare end)

(* 'power state' *)
type pow_s = S.t

(* 'power transition' *)
type pow_t = pow_s * label * pow_s

type pow_s_info =
  { (* Out arcs *)
    outs: pow_t list ;
    
    (* Renaming: its identifier *)    
    id: int }

type pw_acu =
  { (* The deterministic automaton been built. *)
    mm: pow_s_info M.t ;  (* pow_s -> pow_s_info *)
  }

let pows2s p = "{ " ^  sep string_of_int ", " (S.elements p) ^ " }"

let info2s i = Printf.sprintf "( id = %d, %d arcs : %s )" i.id (List.length i.outs) (sep (fun (a,s,b) -> Printf.sprintf "%s --%s--> %s" (pows2s a) s (pows2s b)) ", " i.outs)

let pw2s pw_acu = M.fold (fun pow_s info acu -> acu ^ "   " ^ pows2s pow_s ^ "  -->  " ^ info2s info ^ "\n") pw_acu.mm ""
  
(* Build the out arcs of set pw_s *)
let out_arcs ass (pw_s:pow_s) =

  let rec insert_arc ((_,lbl,b) as arc) (acu:pow_t list) = function
    | [] -> (pw_s, lbl, S.singleton b) :: acu
    | (_,zlbl,zb) as zarc :: rest ->
      if zlbl == lbl || zlbl = lbl then List.rev_append ((pw_s, lbl, S.add b zb) :: rest) acu
      else insert_arc arc (zarc :: acu) rest
    
  in
  
  let insert_st a arcs =
    let a_outs = Assoc.get ass a in
    List.fold_left (fun arcs a_out -> insert_arc a_out [] arcs) arcs a_outs
  in
  
  S.fold insert_st pw_s []

let insert_id pw_acu pw_s =

  let mm = pw_acu.mm in
  assert (not (M.mem pw_s mm)) ;

  let id = M.cardinal mm in
  { mm = M.add pw_s { outs = [] ; id } mm }

let insert_arcs pw_acu pw_s arcs =
  let mm = pw_acu.mm in
  
  match M.find_opt pw_s mm with
  | None -> assert false    
  | Some info ->
    assert (info.outs = []) ;
    { mm = M.add pw_s { outs = arcs ; id = info.id } mm }

let init_pwacu = { mm = M.empty }

let single_0 = S.singleton 0

(*
 * pw_s is the current power_state to explore. 
 *)
let rec build ass pw_acu pw_s =

  if M.mem pw_s pw_acu.mm then pw_acu (* This state is already explored. *)
  else
    (* Add current state now to avoid cycles *)
    let pw_acu2 = insert_id pw_acu pw_s in

    let arcs = out_arcs ass pw_s in
    let pw_acu2 = insert_arcs pw_acu2 pw_s arcs in
   
    let nexts = List.map (fun (_,_,x) -> x) arcs in
    List.fold_left (build ass) pw_acu2 nexts
    


let get_id pw_acu pw_s =
  match M.find_opt pw_s pw_acu.mm with
  | None -> assert false
  | Some info -> info.id

let mk_arc pw_acu id (pw_1, lbl, pw_2) =
  assert (get_id pw_acu pw_1 = id) ;
  let id2 = get_id pw_acu pw_2 in
  (id, lbl, id2)

let mk_state aut (pw_s, info) =
  let state = S.fold (fun i acu -> let st = get_state aut i in if acu = "" then st else acu ^ " ; " ^ st) pw_s "" in
  (info.id, state)

let det aut =

  let ass = to_assoc aut in
  let res = build ass init_pwacu single_0 in

  let _f() = Printf.printf "\n Result : \n%s\n\n%!" (pw2s res) in
  
  (* Build the resulting automaton *)

  let nb_places = M.cardinal res.mm in
  let bindings = M.bindings res.mm in
  
  let states = List.map (mk_state aut) bindings in
  
  let trans = List.flatten (List.map (fun (_, { id ; outs }) -> List.map (mk_arc res id) outs) bindings) in

  { nb_places ; states ; trans }

