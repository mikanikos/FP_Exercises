(** Esercizio 1 **)
type literal = Positive of string | Negative of string;;

(* ??????????????????????????????????? *)

(* consistent: literal list list -> bool *)
(* consistent lista = verifichi se la lista di congiunzioni letterali e' consistente. *)
(* consist : literal list -> bool *)
(* consist lst = true se lst rappresenta una lista consistente
 di letterali *)
let rec consist = function
	| [] -> true
  | (Positive s):: rest -> not (List.mem (Negative s) rest) && consist rest
  | (Negative s):: rest -> not (List.mem (Positive s) rest) && consist rest;;

(* consistent : literal list list -> bool *)
let consistent list = consist (List.flatten list);;

(** Esercizio 2 **)
(* listfrom: 'a -> 'a list -> 'a list *)
(* listfrom x lst = lista che si ottiene da lst eliminando tutti gli elementi che precedono x in lst *)
let rec listfrom x = function
	| [] -> failwith "L'elemento dato non e' presente nella lista"
	| y::rest -> if x = y then y::rest
	             else listfrom x rest;;