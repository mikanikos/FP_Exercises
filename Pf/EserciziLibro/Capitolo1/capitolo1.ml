(** Esercizio 11 **)
(* check_power2: int -> bool *)
(* check_power2 n = verifica se n e' una potenza di 2 *)
let rec check_power2 = function
	| 1 | 2 -> true
	| n -> (n mod 2 = 0) && check_power2 (n/2);;	
(* ALTERNATIVA: (n = 2) || (not(n<2) && (n mod 2 = 0) && check_power2 (n/2)) *)

(** Esercizio 12 **)
(* conta_divisori: int -> int *)
(* conta_divisori n = numero di divisori di n *)
let conta_divisori n =
	let rec aux k cont =
		if k > n then cont
		else if n mod k = 0 then aux (k+1) (cont+1)
		     else aux (k+1) cont
	in aux 1 0;;	

(** Esercizio 13 **)
(* min_divisor: int -> int *)
(* min_divisori m = minimo numero che ha almeno m divisori *)
let min_divisori n =
	let rec nextkdivs (k,n) =
		if conta_divisori (k+1) = n then (k+1)
		else nextkdivs (k+1,n)
	in nextkdivs (0,n);;

(** Esercizio 15 **)
(* is_perfect: int -> bool *)
(* is_perfect n = verifica se n è un numero perfetto *)
let is_perfect n = 
	let rec aux i somma =
		if n = i || n = 1 then somma
		else if n mod i = 0 then aux (i+1) (somma+i)
		     else aux (i+1) somma
	in n = (aux 2 1);;