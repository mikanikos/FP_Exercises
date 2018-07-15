(** ESERCIZIO 1 **)
(* somma_ore: (int * int) -> (int * int) -> int * int *)
(* somma_ore (h1,m1) e (h2,m2) = somma degli orari dati secondo la rappresentazione canonica *)
(* verifica_orario: (int * int) -> bool *)
(* verifica_orario = verifica che il formato dato rappresenti un orario *)
let verifica_orario (h,m) = (h >= 0 && h < 24) && (m >= 0 && m < 60)

let somma_ore (h1,m1) (h2,m2) = 
	if not(verifica_orario (h1,m1) && verifica_orario (h2,m2)) then failwith "Rappresentazione errata di un orario"
	else let (sh,sm) = (h1+h2,m1+m2) in
		   if sm > 59 && sh >= 23 then ((sh+1) mod 24,sm mod 60)
		   else if sh > 23 then (sh mod 24,sm)
		        else if sm >=60 then (sh+1,sm mod 60)
			           else (sh,sm);;

(** ESERCIZIO 2 **)
(* read_max: unit -> int *)
(* read_max = il massimo degli interi letti da tastiera *)
let read_max () =
  let rec loop n =
    try
      let k = read_int()
      in loop (max n k)
    with _ -> n
  in try loop (read_int())
     with _ -> failwith "Sequenza vuota";;

(* read_max_min: unit -> int * int *)
(* read_max_min = il massimo e il minimo degli interi letti da tastiera *)
let read_max_min () =
  let rec loop n m =
    try
      let k = read_int()
      in loop (max n k) (min m k)
    with _ -> (n,m)
  in try let n = read_int()
	       in loop n n
     with _ -> failwith "Sequenza vuota";;

(* tutti_minori: int -> bool *)
(* tutti_minori = un booleano che ci  *)
let rec tutti_minori n =
  try
    let k = read_int()
    in k<n && tutti_minori n
  with _ -> true;;

(* occorre: int -> bool *)
(* occorre = true se n occorre nella sequenza, false altrimenti *)
let rec occorre n =
  try
    let k = read_int()
    in k=n || tutti_minori n
  with _ -> false;;

(* num_di_stringhe: unit -> int *)
(* num_di_stringhe = numero di stringhe immesse da tastiera *)
let rec num_di_stringhe () =
	try
    let s = read_line()
    in if s = "" then 0
		else 1 + num_di_stringhe ()
	with _ -> 0;;

(* stringa_max: unit -> string *)
(* stringa_max = stringa di lunghezza massima *)
let rec stringa_max () =
  let s = read_line ()
  in if s = "" then ""
		 else let s2 = stringa_max ()
	        in if (String.length s2) > (String.length s) then s2
		         else s

(** ESERCIZIO 3 **)
(* sumbetween: int -> int -> int *)
(* sumbetween n m = somma degli interi compresi tra n e m (estremi inclusi) *)
let rec sumbetween n m = 
	if n > m then 0
	else n + sumbetween (n+1) m;;

(* sumto: int -> int *)
(* sumto n = somma degli interi compresi tra 0 e n (incluso), assumendo n > 0 *)
let sumto n = sumbetween 0 n;;

(* variante *)
let rec sumto n =
  if n = 0 then 0
  else n + sumto (n-1);;

(* power : int  ->  int -> int *)
(* power n k = potenza k-esima di n, assumendo k e n non negativi *)
let rec power n k =
	if k = 0 then 1
	else n * power n k-1;;

(* fib: int -> int *)
(* fib n = n-esimo numero di Fibonacci (assumendo n > 0) *)
let rec fib = function
	| 0 -> 0
	| 1 -> 1
	| n -> fib (n-1) + fib (n-2);;

(* maxstring: string -> char *)
(* maxstring s = massimo carattere in s *)
let maxstring s =
	let rec aux n max =
		if n > String.length s - 1 then s.[max]
		else if s.[n] > s.[max] then aux (n+1) n
		     else aux (n+1) max
	in try aux 0 0
	   with _ -> failwith "Stringa vuota";;
		
