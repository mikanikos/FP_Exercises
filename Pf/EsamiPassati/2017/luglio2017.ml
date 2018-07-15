(** Esercizio 1 **)
(* some_all: ('a -> bool) -> 'a list list -> 'a list *)
(* some_all p listona = un elemento (lista) di listona i cui elementi soddisfano tutti il predicato p, un eccezione altrimenti *)
let some_all p listona = 
	try List.find (fun x -> List.for_all p x) listona
	with _ -> failwith "Nessun elemento che soddisfa la condizione";;
