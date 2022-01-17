type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or of 'a bexpr list
                (*| Not of 'a
                  | Implies of 'a*'a*);;

(* On ne peut pas implementer le not avec un simple composant *)

val bool2s : 'a bexpr -> ('a -> string) -> string