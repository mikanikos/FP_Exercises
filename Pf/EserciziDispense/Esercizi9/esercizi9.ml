type 'a ntree = Tr of 'a * 'a ntree list;;

(** ESERCIZIO 1 **)

type multi_expr =
	| MultiInt of int
  | MultiVar of string
  | MultiDiff of multi_expr * multi_expr
  | MultiDiv of multi_expr * multi_expr
  | MultiSum of multi_expr list
  | MultiMult of multi_expr list;;

(* subexpr: multi_expr -> multi_expr -> bool *)
(* subexpr e1 e2 = determina se e2 e'sottoespressione di e1 *)
let rec subexpr e1 e2 =
	e1 = e2 ||
	match e1 with
  | MultiDiff (m1,m2) | MultiDiv (m1,m2) -> subexpr m1 e2 || subexpr m2 e2
  | MultiSum l | MultiMult l -> List.exists (fun x -> subexpr x e2) l
	| _ -> false;;

(* subst: multi_expr -> string -> multi_expr -> multi_expr *)
(* subst e1 x e2 = costruisca l'espressione che si ottiene da E1
sostituendo con E2 ogni occorrenza della variabile di nome x *)
let rec subst e1 x e2 =
	match e1 with
	| MultiInt n -> e1
	| MultiVar y -> if x = y then e2 else e1
	| MultiDiff(m1,m2) -> MultiDiff(subst m1 x e2,subst m2 x e2)
  | MultiDiv(m1,m2) -> MultiDiv(subst m1 x e2,subst m2 x e2)
	| MultiSum l -> MultiSum(List.map (fun e -> subst e x e2) l)
	| MultiMult l -> MultiMult(List.map (fun e -> subst e x e2) l);;


(** ESERCIZIO 2 **)
(* postorder, inorder: 'a ntree -> 'a list *)
(* postorder, inorder t = lista dei suoi nodi nell'ordine in cui sarebbero visitati secondo i due algoritmi di visita *)
let rec postorder (Tr(x,l)) =
	(List.flatten (List.map (postorder) l)) @ [x];;

let rec postorder (Tr(x,l)) =
	postlist x l
and postlist x = function
	| [] -> [x]
	| y::rest -> postorder y @ postlist x rest;;

let rec inorder = function
  | Tr(x,[]) -> [x]
  | Tr(x,t::rest) -> (inorder t) @ x::(List.flatten(List.map inorder rest));;


(** ESERCIZIO 3 **)
(* foglie_in_lista: 'a list -> 'a ntree -> bool *)
(* foglie_in_lista l t = determini se ogni foglia di t appartiene a lst *)
let rec foglie_in_lista l = function
	| Tr(x,[]) -> List.mem x l
	| Tr(x,m) -> List.for_all (foglie_in_lista l) m;;

(** ESERCIZIO 4 **)
(* num_di_foglie: 'a ntree -> int *)
(* num_di_foglie t = numero di foglie di t *)
let rec sumof = function
    [] -> 0
  | x::rest -> x+sumof rest;;

let rec num_di_foglie = function
	| Tr(x,[]) -> 1
	| Tr(x,l) -> sumof (List.map num_di_foglie l);;

(** ESERCIZIO 5 **)
(* listaGuida: 'a list -> 'a ntree -> 'a *)
(* listaGuida l t = la radice del sottoalbero
di T determinato da L, se L determina un sottoalbero di T, un errore altrimenti*)








(** ESERCIZIO 6 **)
(* foglia_costo: int ntree -> (int * int) *)
(* foglia_costo t = l'etichetta e il costo della foglia più costosa dell'albero *)
let rec maxList = function
	| [] -> failwith "errore"
	| [(y,c)] -> (y,c)
	| (y,c1)::(z,c2)::rest -> if c1 > c2 then maxList ((y,c1)::rest)
	                          else maxList ((z,c2)::rest);;

let rec foglia_costo = function
	| Tr(x,[]) -> (x,x)
	| Tr(x,l) -> let (y,costo) = maxList (List.map foglia_costo l)
	             in (y,x+costo);;

(** ESERCIZIO 7 **)
(* tutte_foglie_costi: int ntree -> (int * int) list *)
(* tutte_foglie_costi t = lista di coppie, ciascuna delle quali ha la forma (f,n), dove f è l'etichetta
di una foglia in T e n il costo di tale foglia *)
let rec tutte_foglie_costi = function
	| Tr(x,[]) -> [(x,x)]
	| Tr(x,l) -> List.map (fun (y,c) -> (y, x+c)) (List.flatten (List.map tutte_foglie_costi l));;

(** ESERCIZIO 8 **)
(* ramo_da_lista: 'a ntree -> 'a list -> 'a -> 'a list *)
(* ramo_da_lista t l k = un ramo di T dalla radice a una foglia etichettata da k
che passi per tutti gli elementi di L esattamente una volta e contenga solo
nodi etichettati da elementi di L *)
exception Notfound;;

let rec remove x = function
    [] -> raise Notfound
  | y::rest -> if x=y then rest else y::remove x rest;;

let rec ramo_da_lista t l k =
	match t with
	| Tr(n,[]) -> if n = k && l = [n] then [k] else raise Notfound
	| Tr(x,lst) -> x :: auxlist (remove x l) k lst
and auxlist l k = function
	| [] -> raise Notfound
	| x::rest -> try ramo_da_lista x l k 
	             with _ -> auxlist l k rest;;

(** ESERCIZIO 9 **)
(* ramo_di_primi: int ntree -> int *)
(* ramo_di_primi t = una foglia n dell'albero tale
che il ramo dell'albero dalla radice a n sia costituito da tutti numeri primi *)
let primo n =
  let rec aux = function
      1 -> true
    | k -> (n mod k)<>0 && aux (k-1)
  in n>0 && (n=1 || aux (n/2));;

let rec ramo_di_primi = function
	| Tr(n,[]) -> if primo n then n else raise Notfound
	| Tr(n,l) -> if primo n then auxprimi l
	             else raise Notfound
and auxprimi = function
	| [] -> raise Notfound
	| x::rest -> try ramo_di_primi x
	             with _ -> auxprimi rest;; 

(** ESERCIZIO 10 **)
(* path_non_pred: ('a -> bool) -> 'a ntree -> 'a list *)
(* path_non_pred t = un cammino dalla radice a una foglia di
t che non contenga alcun nodo che soddisfa p *)
let rec path_non_pred p = function
	| Tr(n,[]) -> if not (p n) then [n] else raise Notfound
	| Tr(n,lst) -> if not (p n) then n :: (auxpath p lst) else raise Notfound
and auxpath p = function
	| [] -> raise Notfound
	| x::rest -> try path_non_pred p x 
	             with _ -> auxpath p rest;;

(** ESERCIZIO 11 **)
(* same_structure: 'a ntree -> 'b ntree -> bool *)
(* same_structure t1 t2 = determini se due alberi n-ari hanno la stessa struttura (cioè se essi sono
uguali quando si ignorano le rispettive etichette *)
let rec same_structure t1 t2 =
	let Tr(_,l1) = t1 in
	let Tr(_,l2) = t2 in
	try List.for_all (function (n1,n2) -> same_structure n1 n2) (List.combine l1 l2)
	with _ -> false;;

(** ESERCIZIO 12 **)
type col = Rosso | Giallo | Verde | Blu;;
type 'a col_assoc = (col * 'a list) list;;

(* ramo_colorato: 'a -> 'a col_assoc -> 'a ntree -> 'a list *)
(* ramo_colorato x col t = un ramo a colori alterni, dalla radice dell'albero a una foglia etichettata
da x. Se un tale ramo non esiste, solleverà un'eccezione *)
let rec colore c = function
	| [] -> raise Notfound
	| (x,l)::rest -> if List.mem c l then x else colore c rest;;

let ramo_colorato x ass t =
	let rec aux v = function
		| Tr(n,[]) -> if x = n && (colore n ass) <> v then [x] else raise Notfound
	  | Tr(n,l) -> if (colore n ass) <> v then n :: (try auxcolore (colore n ass) l
		                                               with _ -> raise Notfound)
		             else raise Notfound
   and auxcolore v = function
		| [] -> raise Notfound
	  | n::rest -> try aux v n
		             with _ -> auxcolore v rest
	in match t with
	| Tr(Rosso,_) -> aux Giallo t
	| Tr(Giallo,_) -> aux Verde t
	| Tr(Verde,_) -> aux Blu t
	| Tr(Blu,_) -> aux Rosso t;;


(*let rec path_to y colori (Tr(x,tlist)) =
	if tlist = [] && x = y then [x]
	else x::pathlist (colore x colori) y tlist
and pathlist c y = function
	| [] -> failwith "errore"
	| t::rest -> if c <> colore (root t) colori
               then try path_to y colori t
						        with _ -> pathlist c y rest
							 else pathlist c y rest;;*)
