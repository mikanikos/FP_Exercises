(** Esercizio 1 **)
type ('a,'b) pseudo = ('a list * 'b) list;;

(* dim: 'a -> ('a,'b) pseudo -> 'b *)
(* dim x plist =  dimensione di x secondo plist *)
let rec dim x = function
	| [] -> failwith "Nessun elemento dato nella lista"
	| (lista,d)::rest -> if List.mem x lista then d
	                     else dim x rest;;
