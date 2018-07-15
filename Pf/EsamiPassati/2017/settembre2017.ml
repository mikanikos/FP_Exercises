(** Primo esercizio **)
(* permut: a' list -> a' list -> bool *)
(* permut lista1 lista2 = true se lista1 e' una permutazione di lista2, false altrimenti *)
(* aux: a' list -> a' list -> bool *)
(* aux l1 l2 = true se tutti gli elementi di l1 sono contenuti in l2, false altrimenti *)
let permut lista1 lista2 = 
	let aux l1 l2 =
		List.for_all (fun x -> List.mem x l2) l1
	in (aux lista1 lista2) && (aux lista2 lista1);;