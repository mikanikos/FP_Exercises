(** Esercizio 1 **)
(* precsucc: 'a -> 'a list -> int * int *)
(* precsucc x lst =  coppia (p,s) dove p e' il numero di predecessori di x in lst e s il numero dei suoi successori *)
exception NotFound;;
let precsucc x lst =
  let rec aux (p,s) = function
	  | [] -> raise NotFound
	  | y::rest -> if y=x then (p, List.length rest)
							   else aux (p+1, s) rest
	in aux (0,0) lst;;

(** Esercizio 2 **)
(* precsucc_list: 'a -> 'a list list -> int * int *)
(* precsucc_list x lista = coppia (p,s) dove p e' la somma del numero di predecessori di x in qualche sottolista di lista, e s dei successori *)
(* sum: int list -> int *)
(* sum l = somma degli elementi di l *)
let rec sum = function
  | [] -> 0
  | x::rest -> x + (sum rest);;

let precsucc_list x lista = 
	let rec aux = function
		| [] -> []
		| y::rest -> (try precsucc x y 
		             with NotFound -> (0,0)) :: (aux rest)
	in (sum (List.map fst (aux lista)), sum (List.map snd (aux lista)));; 
 
(*???????????????????????????????????????????????????????????????????????
let precsucc_list x lista = 
	let l = List.map (try precsucc x with | NotFound -> (fun n -> (0,0))) lista
	in (sum (List.map fst l), sum (List.map snd l));; 
?????????????????????????????????????????????????????? *)