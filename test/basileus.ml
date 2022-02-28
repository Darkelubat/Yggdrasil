open Base.Common
open Reader
    
open Aut
    
type stats =
  { (* Ignorable transitions actually removed. *)
    removed_transitions: (string, unit) Hashtbl.t ;

    (* Places that have no input transition *)
    no_input: (int, unit) Hashtbl.t ;
    
    (* Places that have a visible input transition or no input transition at all (init state). *)
    visible_places: (int, unit) Hashtbl.t ;

    (* For each place p2, indicate to which places p1,... their output arcs should be copied
       (because there is a removable transition from p1 to p2). *)
    copy_to: (int, int list) Hashtbl.t }


let init_size = 1000

let init () =
  { removed_transitions = Hashtbl.create init_size ;
    no_input = Hashtbl.create init_size ;
    visible_places = Hashtbl.create init_size ;
    copy_to = Hashtbl.create init_size }

(* Walk through the AUT. *)
let get_stats to_ignore aut =

  let stat_tr st (p1,t,p2) =

    Hashtbl.remove st.no_input p2 ;

    if List.mem t to_ignore then
      begin
        (* Ignorable transition: t has been seen. *)
        Hashtbl.replace st.removed_transitions t () ;

        (* Copy from p2 to p1 *)
        let ll = if Hashtbl.mem st.copy_to p2 then Hashtbl.find st.copy_to p2 else [] in
        Hashtbl.replace st.copy_to p2 (p1 :: ll)        
      end

    else
      (* p2 has a visible input transition. *)
      Hashtbl.replace st.visible_places p2 ()
  in

  let stats = init () in
  let noin p = Hashtbl.replace stats.no_input p () in

  (* Record all places in no_input. *)
  List.iter (fun (p1,_,p2) -> noin p1 ; noin p2) aut.trans ;

  (* *)
  List.iter (stat_tr stats) aut.trans ;

  (* Places with no input are visible. *)
  Hashtbl.iter (fun i () -> Hashtbl.replace stats.visible_places i ()) stats.no_input ;
  
  stats


(* Copy trans (...,t,p2) to p1 
 * visited: list of places p1 already seen in the current recursion, to avoid cycles. *)
let rec copy_trans visited st p1 t p2 acu =

  if List.mem p1 visited then
    (* Cycle ! *)
    acu
      
  else
    (* t visible => p2 visible.  Record output transition of p1 if p1 is visible. *)
    let acu = if Hashtbl.mem st.visible_places p1 then (p1,t,p2) :: acu else acu in

    (* Copy upwards if needed. *)
    if Hashtbl.mem st.copy_to p1 then
      let dests = Hashtbl.find st.copy_to p1 in
      List.fold_left (fun acu dest -> copy_trans (p1 :: visited) st dest t p2 acu) acu dests

    else acu

let insert_trans st acu (p1,t,p2) = if Hashtbl.mem st.removed_transitions t then acu else copy_trans [] st p1 t p2 acu

let copy_aut stats aut = List.fold_left (insert_trans stats) [] aut.trans
  


let go infile ignore outfile =

  let filename = Filename.basename infile in
  
  let to_ignore = String.split_on_char ',' ignore in
  let to_ignore = List.map String.trim to_ignore in
  let to_ignore = List.filter (fun s -> s <> "") to_ignore in

  let aut = Aut.read_aut infile in
  
  (* Pass 1 : record removed transitions *)
  let stats = get_stats to_ignore aut in

  (* Pass 2 : copy transitions *)
  let arcs = copy_aut stats aut in
  let result = Aut.mk_aut arcs in
  
  Aut.write_aut outfile result ;

  (* Check all ignorable transitions are actually in the aut file. *)
  let unseen = List.filter (fun t -> not (Hashtbl.mem stats.removed_transitions t)) to_ignore in
  if unseen = [] then () else Printf.printf "\n  🦂  FYI, these transitions were not found in %s : %s\n\n%!" filename (sep (fun s -> s) ", " unseen) ;
  
  ()


let () =
  match Sys.argv with
  | [| _ ; a ; b ; c |] -> go a b c
                             
  | _ ->
    Printf.printf "\n  🤨  Usage : %s infile \"i1,i2,...\" outfile\n\n%!" Sys.argv.(0) ;
    Printf.printf "         - Reads infile (.aut).\n" ;
    Printf.printf "         - Removes the specified transitions i1, i2, ... , by upmoving or copying visible transitions if needed.\n" ;
    Printf.printf "         - Writes the result in outfile (.aut).\n\n" ;
    ()


    
         
