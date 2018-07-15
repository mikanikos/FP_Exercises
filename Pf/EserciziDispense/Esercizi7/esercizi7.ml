(** ESERCIZIO 1 **)
type direzione = Su | Giu | Destra | Sinistra;;

type posizione = int * int * direzione;;

type azione2 = Gira | Avanti of int;;

(* gira : direzione -> direzione *)
let gira = function
	| Su -> Destra
  | Giu -> Sinistra
  | Destra -> Giu
  | Sinistra -> Su;;

(* avanti : posizione -> int -> posizione *)
let avanti (x,y,dir) n =
	match dir with
	  | Su -> (x,y+n,dir)
    | Giu -> (x,y-n,dir)
    | Destra -> (x+n,y,dir)
    | Sinistra -> (x-n,y,dir);;

(* sposta : posizione -> azione -> posizione *)
let sposta (x,y,dir) act = 
	match act with
	  | Gira -> (x,y,gira dir)
    (* le coordinate non cambiano,
    la direzione gira di 90 gradi in senso orario *)
    | Avanti n -> avanti (x,y,dir) n;;

(* esegui: posizione -> azione list -> posizione *)
(* esegui pos list = la posizione in cui si trova l'oggetto che, trovandosi inizialmente nella posizione
data, esegue in sequenza (e in quest'ordine) le azioni a1,a2,...,an *)
let rec esegui pos = function
	| [] -> pos
	| x::rest -> esegui (sposta pos x) rest;;

(** ESERCIZIO 2 **)
type nat = Zero | Succ of nat;;

(* somma : nat -> nat -> nat *)
let rec somma n m = 
	match n with
	| Zero -> m
	| Succ k -> Succ (somma k m);;

(* int_of_nat : nat -> int *)
(* int_of_nat n = valore intero corrispondente a n *)
let rec int_of_nat = function
	| Zero -> 0
  | Succ n -> succ(int_of_nat n);;

(* prodotto : nat -> nat -> nat *)
(* prodotto n1 n2 = n1 * n2 *)
let rec prodotto n1 n2 =
	match n1 with
	| Zero -> Zero
	| Succ k -> somma n2 (prodotto k n2);;

(** ESERCIZIO 3 **)
type chiave = Aperta | Chiusa;;

type cassaforte = chiave list;;

(* giraPrima: cassaforte -> cassaforte *)
(* giraPrima c = configurazione che si ottiene girando la prima chiave *)
let giraPrima c = if List.hd c = Aperta then Chiusa :: List.tl c else Aperta :: List.tl c;;

(* giraDopoChiusa: cassaforte -> cassaforte *)
(* giraDopoChiusa c = configurazione che si ottiene girando la chiave che segue la prima chiusa*)
let rec giraDopoChiusa = function
	| Chiusa::x::rest -> Chiusa :: giraPrima (x::rest)
	| Aperta::rest -> Aperta::giraDopoChiusa rest
	| _ -> failwith "Nessuna chiave chiusa";;

(* successori: cassaforte -> cassaforte list *)
(* successori c = la lista con le configurazioni (una o due) che si possono ottentere da clist *)
(* con una delle due operazioni (giraPrima e giraDopoChiusa) *)
let successori c =
	try [giraPrima c; giraDopoChiusa c]
	with _ -> [giraPrima c];;

(** ESERCIZIO 4 **)
type obj = Miss | Cann | Barca;;

type situazione = obj list * obj list;;

let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], []);;

type azione = From_left of obj list | From_right of obj list;;

(* safe: situazione -> bool *)
(* safe s = se una situazione e' sicura *)
let conta x lst =
  List.length (List.filter ((=) x) lst);;

let safe s = 
	((conta Cann (fst s) > conta Miss (fst s)) || conta Miss (snd s) = 0) && 
	((conta Cann (snd s) > conta Miss (snd s)) || conta Miss (snd s) = 0);;

(* applica: azione -> situazione -> situazione *)
(* applica a s = la situazione che si ottiene applicando l'azione act a sit *)
exception Fail;;

let verifica_errori sit act = 
	List.length act < 3 &&  act <> [] && List.mem Barca sit && (conta Miss sit >= conta Miss act) && (conta Cann sit >= conta Cann act);;

(* togli_un : 'a -> 'a list -> 'a list *)
(* togli x lst : elimina un'occorrenza di x dalla lista lst *)
let rec togli_un x = function
    [] -> raise Fail
  | y::rest -> 
      if y=x then rest
      else y::togli_un x rest;;

(* togli : 'a list -> 'a list -> 'a list
   togli source lst = elimina da source un'occorrenza di ogni elemento
                      di lst *)
let rec togli source = function
    [] -> source
  | x::rest ->
      togli (togli_un x source) rest;;

let applica a s = 
	match a with
	| From_left l1 -> 
		if verifica_errori (fst s) l1 then raise Fail
		else let nuova1 = (togli_un Barca (togli (fst s) l1), Barca :: l1 @ (snd s))
		     in if safe nuova1 then nuova1
				    else raise Fail
	| From_right l2 -> 
		if verifica_errori (snd s) l2 then raise Fail
		else let nuova2 = (togli_un Barca (togli (snd s) l2), Barca :: l2 @ (fst s))
		     in if safe nuova2 then nuova2
				    else raise Fail;;

let actions =
	let elems =
		[[Miss];[Cann];[Miss;Cann];[Miss;Miss];[Cann;Cann]]
		in (List.map (function x -> From_left x) elems) @ (List.map (function x -> From_right x) elems);;

(* from_sit: situazione -> situazione list *)
(* from_sit sit = lista delle situazioni che si possono ottenere applicando un'azione possibile a sit *)

let from_sit sit = 
	let rec genera = function
		| [] -> []
	  | x::rest -> try (applica x sit)::genera rest with Fail -> genera rest
	in genera actions;;

(** ESERCIZIO 5 **)
type 'a pattern = Jolly | Val of 'a;;

(* most_general_match: 'a list -> 'a list -> 'a pattern list *)
(* most_general_match l1 l2 = lista di pattern con il minor numero possibile di Jolly *)
(* alla quale siano conformi entrambe le liste date *)
let rec most_general_match l1 l2 =
	if List.length l1 <> List.length l2 then failwith "Le liste date hanno lunghezza diversa"
	else 
		match (l1,l2) with
		  | ([],[]) -> []
	    | (x::rest1,y::rest2) -> if (Val x) = (Val y) then (Val x) :: (most_general_match rest1 rest2)
				                       else Jolly :: (most_general_match rest1 rest2)
			| _ -> failwith "Le liste date hanno lunghezza diversa";;