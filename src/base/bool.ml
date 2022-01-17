type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or of 'a bexpr list
                (*| Not of 'a
                  | Implies of 'a*'a*);;

(* On ne peut pas implementer le not avec un simple composant *)

(* let rec bool2s be fu = match be with
| V b -> Printf.sprintf "%s\n" (fu b)
                | And [b::x::[]] -> Printf.sprintf "%s and %s\n" (fu b) (fu x)
                | And [b::x::rest] -> Printf.sprintf "%s and %s and %s\n" (fu b) (fu x) (tos rest fu)
                | Or [b::x::[]] -> Printf.sprintf "%s or %s\n" (fu b) (fu x)
                | Or [b::x::rest] -> Printf.sprintf "%s or %s or %s\n" (fu b) (fu x) (tos rest fu)
           | _ -> raise Failure "not a bexpr";; *)

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

let rec bool2s be fu = match be with
| V b -> Printf.sprintf "%s\n" (fu b)
| And b -> begin match b with
                  | deb::x::[] -> Printf.sprintf "%s and %s\n" (bool2s deb fu) (bool2s x fu)
                  | deb::x::rest -> Printf.sprintf "%s and %s and %s\n" (bool2s deb fu) (bool2s x fu) (bool2s rest fu)
                  end
| Or b-> begin match b with
                  | deb::x::[] -> Printf.sprintf "%s or %s\n" (bool2s deb fu) (bool2s x fu)
                  | deb::x::rest -> Printf.sprintf "%s or %s or %s\n" (bool2s deb fu) (bool2s x fu) (bool2s rest fu)
                  end
| _ -> raise Failure "not a bexpr";;