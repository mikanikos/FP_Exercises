(** ESERCIZIO 1 **)
(* find: ('a -> bool) -> 'a list -> 'a *)
(* find p list = primo elemento di lst che soddisfa il predicato p *)
let rec find p = function
	| [] -> failwith "Nessun elemento con la proprietà voluta"
	| x::rest -> if p x then x else find p rest;;

(* find_applicata: int list -> int *)
(* find_applicata list = primo elemento della lista il cui quadrato sia minore di 30 *)
let find applicata list = find (function a -> a*a <30) list;;

(* takewhile: ('a -> bool) -> 'a list -> 'a list *)
(* takewhile p list = la più lunga parte iniziale di lst costituita tutta da elementi che soddisfano il predicato p *)
let rec takewhile p = function
	| [] -> []
	| x::rest -> if p x then x::takewhile p rest else [];;

(* dropwhile: ('a -> bool) -> 'a list -> 'a list *)
(* dropwhile p lst = la lista che si ottiene eliminando i primi elementi di lst, fino a che soddisfano il predicato p *)
let rec dropwhile p = function
	| [] -> []
	| x::rest -> if p x then dropwhile p rest else x::rest;;

(* partition: ('a -> bool) -> 'a list -> ('a list * 'a list) *)
(* partition p lst = (yes,no), dove yes contiene tutti gli elementi di lst che soddisfano il predicato p, e no quelli che non lo soddisfano *)
let rec partition p = function
	| [] -> ([],[])
	| x::rest -> let (y,n) = partition p rest
               in if p x then (x::y,n)
							    else (y,x::n);;

(* pairwith: 'a -> 'b list -> ('a * 'b) list *)
(* pairwith y [x1;x2;...;xn] = [(y,x1);(y,x2);....; (y,xn)] *)
let pairwith y list = List.map (function x -> (y,x) ) list;;

(* verifica_matrice: int -> int list list -> bool *)
(* verifica_matrice n matr = true se la matrice contiene almeno una riga i cui elementi siano tutti minori di n, false altrimenti *)
let verifica_matrice n matr = List.exists (List.for_all (function x -> x < n)) matr;;

(* setdiff: 'a list -> 'a list -> 'a list *)
(* setdiff l1 l2 = differenza insiemistica *)
let setdiff l1 l2 = List.filter (function x -> not (List.mem x l2)) l1;;

(* subset: 'a list -> 'a list -> bool *)
(* subset set1 set2 = true se set1 rappresenta un sottoinsieme di set2 *)
let subset set1 set2 = List.for_all (function x -> (List.mem x set2)) set1;;

(* duplica: int list -> int list *)
(* duplica list = raddoppia tutti gli elementi di una lista di interi, usando la funzione List.map *)
let duplica list = List.map (function x -> 2*x) list;;

(* mapcons: ('a * 'b list) list -> 'b -> ('a * 'b list) list *)
(* mapcons l x = lista che si ottiene inserendo x in testa a ogni secondo elemento delle coppie in L *)
let mapcons l x = List.map (function (s,y) -> (s,x::y)) l;;

(* tutte_liste_con: int -> 'a -> 'a -> 'a list list *)
(* tutte_liste_con n x y = lista contenente tutte le possibili liste di lunghezza n contenenti soltanto i due valori x e y *)

(* cons: 'a -> 'a list -> 'a list*)
let cons x rest = x::rest;;

let rec tutte_liste_con n x y =
	if n = 0 then [[]]
	else let tmp = tutte_liste_con (n-1) x y
       in (List.map (cons x) tmp) @ (List.map (cons y) tmp);;

(* interleave: 'a -> 'a list -> 'a list list *)
(* interleave x lst = lista con tutte le liste che si ottengono inserendo x in qualsiasi posizione in lst *)
let rec interleave x lst =
	match lst with
	| [] -> [[x]]
	| y::rest -> (x::y::rest) :: (List.map (cons y) (interleave x rest));;

(* permut: 'a list -> 'a list list *)
(* permut lst riporti una lista con tutte le permutazioni di lst *)
let rec permut = function
	| [] -> [[]]
	| x::rest -> List.flatten (List.map (interleave x) (permut rest));;


(** ESERCIZIO 2 **)
(* esempio di matrice con contenuti di tipo 'a  *)
type 'a mat = int * ((int * int) * 'a ) list
let labirinto = (5,
     [((1,0),"oro");
      ((3,1),"oro");
      ((4,3),"oro");
      ((0,1),"argento");
      ((2,4),"argento");
      ((0,2),"mostro");
      ((1,1),"mostro");
      ((1,3),"mostro");
      ((2,3),"mostro");
      ((3,0),"mostro");
      ((4,2),"mostro")]);;

(* in_riga: (int * ((int * int) * 'a) list) -> int -> 'a -> bool *)
(* in_riga m n v = verifica se la riga data contiene il valore dato *)
let in_riga m n v = 
	if n > fst m || n < 0 then failwith "Riga non presente"
	else let l = snd m
	     in List.exists (function ((a,_),t) -> a = n && t = v) l;;

(* trova_colonna: (int * ((int * int) * 'a) list) -> int -> 'a *)
(* trova_colonna m n v = il numero di colonna c tale che (r,c) contiene v (se esiste), altrimenti sollevi un'eccezione *)
let trova_colonna m n v =
	if n > fst m || n < 0 then failwith "Riga non presente"
	else let l = snd m
	     in snd (fst (List.find (function ((a,_),t) -> a = n && t = v) l));;

(* in_tutte: (int * ((int * int) * 'a) list) -> 'a -> bool *)
(* in_tutte m v = verifica se tutte le righe della matrice contengono il valore dato *)
let rec upto inizio fine =
  if inizio > fine then []
  else inizio:: upto (inizio+1) fine;;

let in_tutte2 m v =
	let (dim,_) = m in
  List.for_all (function r -> in_riga m r v) (upto 0 (dim-1));;


(** ESERCIZIO 3 **)
(* find2: 'a -> 'a list -> 'a list * 'a list *)
(* find2 x l = coppia di liste dove la prima contiene tutti gli elementi che vanno dall'inizio della lista fino alla prima *)
(* occorrenza di x esclusa; la seconda contiene tutti gli elementi che seguono la prima occorrenza di x *)
let rec find2 x = function
	| [] -> failwith "Errore"
  | y::rest -> 
      if x=y then ([],rest)
      else let (prima,dopo) = find2 x rest
      in (y::prima,dopo);;

(* spezza: 'a -> 'a list -> 'a list * 'a list *)
(* spezza x l = coppia di liste (L1,L2), dove L1 contiene tutti gli elementi di l che vanno dalla prima alla seconda occorrenza
   di x, estremi esclusi, e L2 contiene tutti gli elementi di l che seguono la seconda occorrenza di x *)
let rec spezza x = function
	| [] -> failwith "Non sono presenti almeno due occorrenze nella lista data"
	| y::rest -> 
		if x = y then find2 x rest
		else spezza x rest;;

(* prendi: ('a -> bool) -> 'a list -> 'a * 'a list *)
(* prendi p l = solleva un'eccezione se l non contiene alcun elemento che soddisfa p, altrimenti restituisce una coppia (x,L0)
dove x è un elemento di L che soddisfa p e L0 contiene tutti gli altri elementi di L, in qualsiasi ordine *)
let rec cancella x = function
	| [] -> []
	| y::rest -> 
		if x = y then rest
		else y :: cancella x rest;; 
		
let prendi p l = 
	let k = List.find p l
	in (k, cancella k l);;
