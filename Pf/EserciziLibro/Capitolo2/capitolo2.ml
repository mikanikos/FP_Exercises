(** Esercizio 22 **)
(* ordina_coppie: (int * int) list -> (int * int) list *)
(* ordina_coppie lst = ordina lst secondo l'ordine: (x,y) < (z,w) se x^2 - y^2 < z^2 - w^2 *)
let ordina_coppie lst = List.sort (fun (x,y) (z,w) -> if x*x - y*y < z*z - w*w then -1 else 1) lst;;

(** Esercizio 23 **)
(* ordina_stringhe: string list -> string list *)
(* ordina_stringhe lst = ordina lst secondo la lunghezza delle stringhe *)
let ordina_stringhe lst = List.sort (fun s1 s2 -> compare (String.length s1) (String.length s2)) lst;;