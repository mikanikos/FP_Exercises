(** ESERCIZIO 1 **)
(* ultime_cifre: int -> int * int *)
(* ulime_cifre n = coppia con penultima e ultima cifra di n (0 se non esiste) *)
	let ultime_cifre n =
    let m = abs n
		in ((m/10) mod 10, m mod 10);;
		
(** ESERCIZIO 2 **)
(* bello: int -> bool *)
(* bello n = true se n numero bello (ultima cifra bella (0,3,7) e penultima (se esiste) no), false altrimenti *)
let bello n =
    let (pen,ult) = ultime_cifre n
    in let isCifraBella n =
         match abs n with
           | 0 | 3 | 7 -> true
           | _ -> false
    in if abs n = ult then isCifraBella n
       else not(isCifraBella pen) && isCifraBella ult;;

(** ESERCIZIO 3 **)
(* data: int * string -> bool *)
(* data (d,m) = true se la coppia è una data corretta di giorno e mese secondo le regole classiche *)
	let data (d,m) =
		match m with
		| "febbraio" -> d >= 1 && d <= 28
		| "aprile" |"giugno" | "settembre" | "novembre" -> d >= 1 && d <= 30
		| "gennaio" | "marzo" | "maggio" | "luglio" | "agosto" | "ottobre" -> d >= 1 && d <= 31 
		| _ -> false;;
