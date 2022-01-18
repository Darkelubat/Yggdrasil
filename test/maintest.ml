open Base

open Props
open Propsprint
    
let its1 = State "s1"
let its2 = State "s2"
let ite1 = Event "e1"
let ite2 = Event "e2"

let int1 = Time (0,10)

let path0 = Path []
let path1 = Path [ ite1 ; its1 ]
let path2 = Path [ ite1 ; ite2 ]

let tcons1 = Between (its1, ite2)
let tcons2 = Notc

let obs1 = Enter (Atom (path1, its2))
let obs2 = Leave (Atom (path0, ite2))
let obs3 = Atom (path0, its1)

let pat1 = Leadsto (obs1, obs2)
let pat2 = Always obs3

let pdef1 = Pattern (pat1, tcons1)
let pdef2 = Pattern (pat2, tcons2)

let pdef3 = Bool (Or [ V pdef1 ; V pdef2 ])


let test1 () =
  Printf.printf "Test 1 : %s\n%!" (pdef2s pdef3) ;

  ()



let () = test1 ()
    
