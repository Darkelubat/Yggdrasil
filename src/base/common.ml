(* Utils *)

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

