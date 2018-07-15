(** ESERCIZIO 1 **)
(* length: 'a list -> int *)
(* length list = numero di elementi in una lista *)
let rec length = function
	| [] -> 0
	| x::rest -> 1 + length rest;;

(* sumof: int list -> int *)
(* sumof list = somma degli elementi in una lista di interi *)
let rec sumof = function
	| [] -> 0
	| x::rest -> x + sumof rest;;

(* maxlist: 'a list -> 'a *)
(* maxlist list = massimo elemento in una lista *)
let rec maxlist = function
    [] -> failwith "Lista vuota"
  | [x] -> x
  | x::rest -> max x (maxlist rest)

let maxlist_it list = 
	let rec aux m = function
		| [] -> m
		| x:: rest -> aux (max m x) rest
	in try 
		   aux (List.hd list) list
	   with _ -> failwith "Lista vuota";;

(* drop: int -> 'a list -> 'a list *)
(* drop n lst = lista che si ottiene da lst togliendone i primi n elementi *)
let rec drop n lst =
	if n <= 0 then lst
	else 
		match lst with
		| [] -> []
	  | x::rest -> drop (n-1) rest;;

(* append: 'a list -> 'a list -> 'a list *)
(* append lst1 lst2 = lista unica composta con elementi di lst1 e lst2 *)
let rec append lst1 lst2 =
	match lst1 with
	| [] -> lst2
	| x::rest -> x::(append rest lst2);;

(* reverse: 'a list -> 'a list *)
(* reverse lst = lst rovesciata *)
let rec reverse = function
	| [] -> []
	| x::rest -> (reverse rest) @ [x];;

(* nth: int -> 'a list -> 'a *)
(* nth n lst = elemento di lst in posizione n, dove il primo elemento della lista è in posizione 0 *)
let rec nth n = function
	[] -> failwith "Errore"
  | x::rest -> if n=0 then x else nth (n-1) rest 	

(* remove: 'a -> 'a list -> 'a list *)
(* remove x lst = lst senza tutte le occorrenze di x *)
let rec remove x = function
	| [] -> []
	| n::rest -> if n = x then remove x rest else n::(remove x rest);;

(** ESERCIZIO 2 **)
(* copy: int -> 'a -> 'a list *)
(* copy n x = la lista di lunghezza n i cui elementi sono tutti uguali a x *)
let rec copy n x =
	if n = 0 then []
	else x :: (copy (n-1) x);;

(* nondec: int list -> bool *)
(* nondec lst = true se gli elementi di lst sono in ordine non decrescente, false altrimenti *)
let rec nondec = function
	| [] -> true
	| [x] -> true
	| x :: (y :: rest) -> x <= y && nondec (y::rest);;

(* pairwith: 'a -> 'b list -> ('a * 'b) list *)
(* pairwith y lst = applicata ad un valore y e una lista xs = [x1;x2;...;xn], riporti la lista [(y,x1);(y,x2);....;(y,xn)] *)
let rec pairwith y = function
	| [] -> []
	| x::rest -> (y,x) :: pairwith y rest;;

(* duplica: 'a list -> 'a list *)
(* duplica list = applicata a una lista xs = [x1;x2;...;xn], riporti la lista [x1;x1;x2;x2;...;xn;xn] *)
let rec duplica = function
	| [] -> []
	| x::rest -> x :: (x :: duplica rest);;

(* enumera: 'a list -> (int * 'a) list *)
(* enumera list = applicata a una lista lst=[x0;x1;x2;...;xk], riporti la lista di coppie [(0,x0);(1,x1);(2,x2);...;(k,xk)] *)
let enumera list = 
	let rec aux n = function
		| [] -> []
		| x::rest -> (n,x) :: aux (n+1) rest
	in aux 0 list;;

(* position: 'a -> 'a list -> int *)
(* position x lst riporti la posizione della prima occorrenza di x in lst (contando a partire da 0) *)
let position x lst = 
	let rec aux n = function
	  | [] -> failwith "Valore non presente"
	  | y::rest -> if y=x then n else aux (n+1) rest
	in aux 0 lst;;

(* alternate: 'a list -> 'a list *)
(* alternate lst = applicata a una lista lst, riporti la lista contentente tutti e soli gli elementi di lst che si trovano in posizione dispari *)
let alternate lst = 
	let rec aux n = function
	  | [] -> []
	  | x::rest -> if n mod 2=0 then aux (n+1) rest else x :: (aux (n+1) rest)
	in aux 0 lst;;

(* min_dei_max: int list list -> int *)
(* min_dei_max list = data una lista di liste di interi, riporti il valore minimo tra i massimi di ciascuna lista *)
let rec min_dei_max = function
	| [] -> failwith "Nessun valore nella lista"
	| [x] -> maxlist x
	| y::rest -> min (maxlist y) (min_dei_max rest);;

(* split2: 'a list -> 'a list * 'a list *)
(* slipt2 list = restituisce due liste di lunghezza più o meno uguale mettendo i primi elementi da una parte e gli ultimi dall'altra *)
(* take: int -> 'a list -> 'a list *)
(* take n lst = primi n elementi di lst *)
let rec take n = function
    [] -> []
  | x::rest -> 
      if n<=0 then []
      else x::(take (n-1) rest);;

let split2 list =
	let n = (List.length list) / 2
	in (take n list, drop n list);;
