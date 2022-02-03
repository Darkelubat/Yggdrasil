type path = string

type label = string

type arc = int * label * int

type aut =
  { nb_places: int ;
    nb_trans: int ;
    trans: arc list }

let mk_aut arcs =
  let nb_places = 1 + List.fold_left (fun acu (p1,_,p2) -> max (max acu p1) p2) 0 arcs in
  { nb_places ; nb_trans = List.length arcs ; trans = arcs }

let read_aut path =
  let inch = Scanf.Scanning.open_in path in
  try

    (* First line *)
    let (p,t) = Scanf.bscanf inch " des( %_d , %d , %d )\n" (fun p t -> (p,t)) in

    let aut = { nb_places = p ;
                nb_trans = t ;
                trans = [] }
    in    
    
    let rec loop aut =
      let line =
        try Scanf.bscanf inch " ( %d , \"%s@\" , %d )" (fun a b c -> Some (a,b,c))
        with End_of_file -> None
      in

      match line with
      | None -> aut
      | Some (a,b,c) -> loop { aut with trans = (a,b,c) :: aut.trans }
    in

    let res = loop aut in    
    Scanf.Scanning.close_in inch ;

    let aut = mk_aut res.trans in
    Printf.printf "\n  👍  File %s read : %d places, %d transitions.\n\n%!" path aut.nb_places aut.nb_trans ;

    if aut.nb_trans <> t  then
      Printf.printf "  ⚠  The number of transitions specified in des(_,%d,_) does not match the number of transitions found in the file (%d).\n" t aut.nb_trans ;
    
    if aut.nb_places <> p then
      Printf.printf "  ⚠  The number of places specified in des(_,_,%d) does not match the number of placs found in the file (%d).\n" p aut.nb_places ;

    aut
    
  with e ->
    Scanf.Scanning.close_in inch ;
    raise e
     
let write_aut path aut =
  let out = open_out path in

  Printf.fprintf out "des(0,%d,%d)\n" aut.nb_trans aut.nb_places ;

  let arcs = List.sort Stdlib.compare aut.trans in
  List.iter (fun (a,b,c) -> Printf.fprintf out "(%d,\"%s\",%d)\n" a b c) arcs ;
  
  close_out out ;

  Printf.printf "\n  👌  File %s written : %d places, %d transitions.\n\n%!" path aut.nb_places aut.nb_trans ;
  ()
  
