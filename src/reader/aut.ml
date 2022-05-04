type path = string

type label = string

type arc = int * label * int

type state = string

type aut =
  { nb_places: int ;
    states: (int * state) list ;
    trans: arc list }

(* Indicates if string b starts with string a, that is, a is a prefix of b. *)
let starts_with a b =
  let l = String.length a in
  (String.length b >= l) && (String.sub b 0 l = a)

let mk_aut states arcs =
  let nb_places = 1 + List.fold_left (fun acu (p1,_,p2) -> max (max acu p1) p2) 0 arcs in
  { nb_places ; trans = arcs ; states }

let read_aut path =
  let inch = Scanf.Scanning.open_in path in
  try

    (* First line *)
    let p = Scanf.bscanf inch " des( %_d , %_d , %d )\n" (fun p -> p) in

    let aut = { nb_places = p ;
                states = [] ;
                trans = [] }
    in    
    
    let rec loop aut =
      let line =
        try Scanf.bscanf inch " ( %d , \"%s@\" , %d )" (fun a b c -> Some (a,b,c))
        with End_of_file -> None
      in

      match line with
      | None -> aut
      | Some (a,b,c) ->

        let aut2 =        
          (* Look at label. *)
          if starts_with "E.`" b then
            (* Remove last ` *)
            let lbl =
              let len = String.length b in
              let rlen = if b.[len-1] = '`' then len-1 else len in
              String.sub b 3 (rlen-3)
            in
            { aut with trans = (a,lbl,c) :: aut.trans }

          else if a = c && starts_with "S.`" b then
            (* State *)
            { aut with states = (a,b) :: aut.states }
            
          else
            (* Standard transition label. *)
            { aut with trans = (a,b,c) :: aut.trans }
        in

        loop aut2
    in

    let res = loop aut in    
    Scanf.Scanning.close_in inch ;

    let aut = mk_aut res.states res.trans in
    Printf.printf "\n  👍  File %s read : %d places, %d transitions.\n%!" path aut.nb_places (List.length aut.trans) ;

    if aut.nb_places <> p then
      Printf.printf "  ⚠  The number of places specified in des(_,_,%d) does not match the number of places found in the file (%d).\n" p aut.nb_places ;

    aut
    
  with e ->
    Scanf.Scanning.close_in inch ;
    raise e

type format = AUT | AUT_SP2

let write_aut format path aut =
  let out = open_out path in

  let n_trans = List.length aut.trans in
  
  let lines = match format with
    | AUT -> n_trans
    | AUT_SP2 -> n_trans + aut.nb_places
  in
  
  Printf.fprintf out "des(0,%d,%d)\n" lines aut.nb_places ;

  (* Output places *)
  let put_state (n,st) = Printf.fprintf out "(%d,\"%s\",%d)\n" n st n in
  
  let () = match format with
    | AUT -> ()
    | AUT_SP2 ->
      List.iter put_state (List.sort Stdlib.compare aut.states)
  in

  (* Output transitions *)
  let arcs = List.sort Stdlib.compare aut.trans in

  let put_trans = match format with
    | AUT -> (fun (a,b,c) -> Printf.fprintf out "(%d,\"%s\",%d)\n" a b c)
    | AUT_SP2 -> (fun (a,b,c) -> Printf.fprintf out "(%d,\"E.`%s`\",%d)\n" a b c)
  in
  List.iter put_trans arcs ;
  
  close_out out ;

  Printf.printf "\n  👌  File %s written : %d places, %d transitions.\n\n%!" path aut.nb_places n_trans ;
  ()
  
let to_assoc aut =
  let tbl = Assoc.create ~size:(120 * aut.nb_places / 100) ~init:(fun _ -> []) () in
  List.iter (fun ((a,_,_) as arc) -> Assoc.update tbl a (fun l -> arc :: l)) aut.trans ;
  tbl

let get_state aut i =
  match List.assoc_opt i aut.states with
  | Some x -> x
  | None -> ""
    (* Printf.printf "\n  ❌  Error : state %d not found in automaton (Aut.get_state).\n\n%!" i ; assert false *)
