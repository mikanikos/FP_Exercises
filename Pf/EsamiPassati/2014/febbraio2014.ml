(** Esercizio 1 **)
(* ordinati: 'a -> 'a list -> 'a list *)
(* ordinati start input = il primo elemento di output e' il primo elemento di input maggiore
o uguale a start (se input contiene un tale elemento); gli elementi
successivi di output si ottengono eliminando da input tutti quelli
che non renderebbero output ordinata *)
let rec ordinati start = function
	| [] -> []
	| x::rest -> if x >= start then x :: (ordinati x rest)
	             else ordinati start rest;;