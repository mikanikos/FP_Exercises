(** ESERCIZIO 1 **)
(* filter_vicini int -> (int * int) list -> (int * int) list *)
(* filter_vicini dim lista_caselle = elementi di lista_caselle che sono interne al labirinto di dimensione dim *)
(* aux: (int * int) list -> (int * int) list -> (int * int) list *)
(* aux result clist = (coppie in clist che corrispondono a caselle nel labirinto) @ result *)
(*  in_labirinto : int -> int * int -> bool *)
let in_labirinto dim (r,c) =
  r >= 0 && c >= 0 && r < dim && c < dim;;

let filter_vicini dim lista =
	let rec aux result = function
		| [] -> result
		| x::rest -> if in_labirinto dim x then aux (x::result) rest else aux result rest
	in aux [] lista;;

(* raccolti : ('a * 'b list) list -> 'b list -> 'a list *)
(* raccolti contents caselle = lista di tutti gli oggetti contenuti in qualche casella della lista caselle, secondo la lista *)
(* associativa contents. La lista puo' avere ripetizioni  *)
(* aux: 'a list -> 'b list -> 'a list *)
(* aux result clist = lista con il contenuto delle caselle in clist concatenato con result *)
(*  find_content : ('a * 'b list) list -> 'b -> 'a list *)
let rec find_content contenuti c =
  match contenuti with
    [] -> []
  | (x,caselle)::rest ->
      if List.mem c caselle then x::find_content rest c
      else find_content rest c;;

let raccolti contents caselle =
	let rec aux result = function
		| [] -> result
		| x::rest -> aux ((find_content contents x) @ result) rest
  in aux [] caselle;;

(* combine: 'a list -> 'b list -> ('a * 'b) list *)
(* combine [x1;x2;...;xn] [y1;y2;...;yn] = [ (x1,y1); (x2,y2); .... (xn,yn) ] *)
let rec combine l1 l2 = 
	match (l1,l2) with
	| ([],[]) -> []
	| (x::rest1,y::rest2) -> (x,y)::combine rest1 rest2
	| _ -> failwith "Liste di diversa lunghezza";;


(* split: ('a * 'b) list -> 'a list * 'b list *)
(* split [(x1,y1); (x2,y2); .... (xn,yn)] = ([x1;x2;...;xn],[y1;y2;...;yn]) *)
let rec split = function
	| [] -> ([],[])
	| (x,y)::rest -> let (list1,list2) = split rest	
                   in (x::list1,y::list2);;

(* cancella: 'a -> ('a * 'b) list -> ('a * 'b) list *)
(* cancella k list = list senza tutte le coppie che hanno come primo elemento la chiave data *)
let rec cancella k = function
	| [] -> []
	| (x,y)::rest -> if x = k then cancella k rest
	                 else (x,y)::cancella k rest;;

(** ESERCIZIO 2 *)
(* unione: 'a list -> 'a list -> 'a list *)
(* unione l1 l2 = unione dei due insiemi rappresentati da liste *)
let rec union l1 = function
  | [] -> l1
  | x::rest -> if List.mem x l1 then union l1 rest
               else x::union l1 rest;;

(* diff: 'a list -> 'a list -> 'a list *)
(* diff l1 l2 = differenza dei due insiemi rappresentati da liste *)
let rec diff l1 l2 = 
	match l1 with
	| [] -> []
	| x::rest -> if List.mem x l2 then diff rest l2
	             else x::diff rest l2;;

(* inters: 'a list -> 'a list -> 'a list *)
(* inters l1 l2 = intersezione dei due insiemi rappresentati da liste *)
let rec inters l1 = function
	| [] -> []
	| x::rest -> if List.mem x l1 then x::inters l1 rest
	             else inters l1 rest;;

(* subset: 'a list -> 'a list -> bool *)
(* subset set1 set2 = true se set1 e' sottoinsieme di set2 *)
let rec subset set1 set2 =
	match set1 with
	| [] -> true
	| x::rest -> List.mem x set2 && subset rest set2;;

(** ESERCIZIO 3 **)
(* explode: string -> char list *)
(* explode s = restituisce una lista con i caratteri di s *)
let explode s =
	let rec aux i result =
		if i < 0 then result
		else aux (i-1) (s.[i]::result)
	in aux (String.length s -1) [];;

(* implode: char list -> string *)
(* implode list = restituisce una stringa con i caratteri di list *)
let rec implode = function
	| [] -> ""
	| x::rest -> (String.make 1 x) ^ implode rest;;

(** ESERCIZIO 4 **)
(* intpairs: int -> (int*int) list *)
(* intpairs n = lista di tutte le coppie di interi (x,y) con x e y compresi tra 1 e n *)
(* sugg: 'a -> 'b list ->  ('a * 'b) list *)
(* sugg y list = lista di tutte le coppie di interi (y,x) con x tutti gli elementi della lista *)
(* upto : int -> int list *)
(* upto n = [n;n-1;...;1] *)
let rec sugg y = function 
	| [] -> []
	| x::rest -> (y,x)::sugg y rest;;

(* upto : int -> int list, 
   upto n = [n;n-1;...;1] *)
let rec upto n =
  if n <= 0 then []
  else n::upto(n-1)

let intpairs n =
	let range = upto n in
  let rec aux = function
   | [] -> []
   | x::rest -> (sugg x range) @ aux rest
  in aux range;;

(** ESERCIZIO 5 **)
(* trips: 'a list -> ('a * 'a * 'a) list *)
(* trips lst = la lista di tutte le triple adiacenti di elementi di lst *)
let rec trips = function
	| x::y::z::rest -> (x,y,z)::trips(y::z::rest)
  | _ -> [];;

(** ESERCIZIO 6 **)
(* choose: int -> 'a list -> 'a list list *)
(* choose k lst = una lista contenente tutte le sottoliste di L di lunghezza k *)
(* take: int -> 'a list -> 'a list *)
let rec take n = function
	| [] -> []
  | x::rest -> 
      if n=0 then []
      else x::take (n-1) rest;;

let rec choose k lst =
	if k > List.length lst then []
	else (take k lst) :: choose k (List.tl lst);;

(** ESERCIZIO 7 **)
(* strike_ball: 'a list -> 'a list -> (int * int) *)
(* strike_ball test guess = una coppia (strike,ball) dove strike è il numero di elementi di test
che occorrono anche in guess, ma in diversa posizione, e ball è il numero di
elementi di test che occorrono in guess nella stessa posizione in cui sono in test *)
let strike_ball test guess =
  let rec aux = function
    | ([],[]) -> (0,0)
    | (x::target,y::lst) -> 
  let (strike,ball) = aux(target,lst)
  in if x=y then (strike,ball+1)
     else if List.mem y test then (strike+1,ball)
          else (strike,ball)
	  |_-> failwith "lunghezza diversa"
  in aux (test,guess);;

(** ESERCIZIO 8 **)
(* insert: int -> int list -> int list *)
(* insertion_sort: int list -> int list *)
let rec insert x = function
	| [] -> [x]
	| y::rest -> if x <=y then x::y::rest else y::(insert x rest);;

let rec insertion_sort = function
	| [] -> []
	| x::rest -> insert x (insertion_sort rest);; 
		

(* partition : 'a list -> 'a -> 'a list * 'a list *)
(* quick_sort: 'a list -> 'a list *)
let rec partition lst pivot = 
	match lst with
	| [] -> ([],[])
  | x::rest -> let (left,right) = partition rest pivot
             in if x<=pivot then (x::left,right) else (left,x::right);;

let rec quick_sort = function
	| [] -> []
	| x::rest -> let (left,right) = partition rest x
               in (quick_sort left) @ (x::quick_sort right);;
