(** Esercizio 1 **)
(* purge: 'a -> 'a list list -> 'a list list *)
(* purge x lista_di_liste = la lista contenente (in qualsiasi ordine) tutti e solo gli elementi di lista_di_liste che non contengono x *)
let purge x lista_di_liste = List.filter (fun l -> not (List.mem x l)) lista_di_liste;;