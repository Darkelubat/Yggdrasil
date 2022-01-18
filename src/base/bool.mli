

(* Boolean composition of 'a predicates. *)

type 'a bexpr = | V of 'a
                | And of 'a bexpr list
                | Or of 'a bexpr list
                (*| Not of 'a
                  | Implies of 'a*'a*);;

(* On ne peut pas implementer le not avec un simple composant - à voir plus tard. *)

val bool2s : ('a -> string) -> 'a bexpr -> string
