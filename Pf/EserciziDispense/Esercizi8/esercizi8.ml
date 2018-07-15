(** ESERCIZIO 1 **)

type expr =
	| Jolly
	| Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr;;

(* subexpr: expr -> expr -> bool *)
(* subexpr e1 e2 = date due espressioni aritmetiche E1 e E2 determini se E2 è una sottoespressione di E1 *)
let rec subexpr e1 e2 =
	e1 = e2 ||
	match e1 with
  | Sum(s1,s2) | Diff(s1,s2) | Mult(s1,s2) | Div(s1,s2) -> (subexpr s1 e2) || (subexpr s2 e2)
	| _ -> false;;

(* subst_in_expr: expr -> string -> expr -> expr *)
(* subst_in_expr e1 x e2 = l'espressione che si ottiene da E sostituendo ogni occorrenza di x con E' *)
let rec subst_in_expr e1 x e2 =
	match e1 with
	| Int n -> e2
	| Var y -> if y = x then e2 else e1
	| Sum(a,b) -> Sum(subst_in_expr a x e2,subst_in_expr b x e2)
	| Diff(a,b) -> Diff(subst_in_expr a x e2,subst_in_expr b x e2)
	| Mult(a,b) -> Mult(subst_in_expr a x e2,subst_in_expr b x e2)
	| Div(a,b) -> Div(subst_in_expr a x e2,subst_in_expr b x e2);;

(** ESERCIZIO 2 **)
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

(* reflect : 'a tree -> 'a tree *)
(* reflect t = immagine riflessa di t *)
let rec reflect = function
	| Empty -> Empty
	| Tr(x,f1,f2) -> Tr(x, reflect f2, reflect f1);;

(* fulltree : int -> int tree *)
(* fulltree n = albero binario completo di altezza n, con i nodi etichettati da interi
come segue: la radice è etichettata da 1, i figli di un nodo etichettato da k
sono etichettati da 2k e 2k + 1 *)
let rec fulltree n =
	let rec aux n k =
		if n = 0 then Empty
		else Tr(k, aux (n-1) (2*k), aux (n-1) ((2*k)+1))
	in aux n 1;;

(* balanced: 'a tree -> bool *)
(* balanced t = determina se un albero e' bilanciato o meno *)
(* altezza: 'a tree -> int 
   altezza t = altezza di t *)
let rec altezza = function
	| Empty -> 0
	| Tr(_,t1,t2) -> 1 + max (altezza t1) (altezza t2);;

let rec balanced = function
	| Empty -> true
	| Tr(_,t1,t2) -> abs (altezza t1 - altezza t2) <= 1 && balanced t1 && balanced t2;;

(* preorder, postorder, inorder: 'a tree -> 'a list *)
(* preorder, postorder, inorder: 'a tree -> 'a list *)
(* preorder, postorder, inorder t = lista dei nodi di t, nell'ordine in
cui sarebbero visitate secondo gli algoritmi di visita*)
let rec preorder = function
	| Empty -> []
	| Tr(x,t1,t2) -> x::(preorder t1 @ preorder t2);;

let rec postorder = function
	| Empty -> []
	| Tr(x,t1,t2) -> (postorder t1) @ ((postorder t2) @ [x]);;

let rec inorder = function
    Empty -> []
  | Tr(x,t1,t2) -> (inorder t1) @ (x::(inorder t2));;

(* balpreorder, balinorder: 'a list -> 'a tree *)
(* balpreorder, balinorder l = costruiscono un albero bilanciato con nodi etichettati da elementi di lst *)
(* take : int -> 'a list -> 'a list *)
let rec take n = function
    [] -> []
  | x::xs -> if n<=0 then []
             else x::take (n-1) xs;;

(* drop : int -> 'a list -> 'a list *)
let rec drop n = function
    [] -> []
  | x::xs -> if n<=0 then x::xs
             else drop (n-1) xs;;

let rec balpreorder = function
	| [] -> Empty
	| x::rest -> Tr(x, balpreorder (take ((List.length rest)/2) rest), balpreorder (drop ((List.length rest)/2) rest));;

(* balinorder : 'a list -> 'a tree *)
let rec balinorder = function
    [] -> Empty
  | xs -> let k = (List.length xs)/ 2
          in let (y::ys) = drop k xs
          in Tr(y, balinorder (take k xs),
                   balinorder ys);;

(** ESERCIZIO 3 **)
(* foglie_in_lista: 'a list -> 'a tree -> bool *)
(* foglie_in_lista lst t = determini se ogni foglia di t appartiene a lst *)
let rec foglie_in_lista lst = function
	| Empty -> true
	| Tr(x,Empty,Empty) -> List.mem x lst
	| Tr(x,l,r) -> foglie_in_lista lst l && foglie_in_lista lst r;;

(** ESERCIZIO 4 **)
(* num_foglie: 'a tree -> int *)
(* num_foglie t = numero di foglie dell'albero *)
let rec num_foglie = function
	| Empty -> 0
	| Tr(x,Empty,Empty) -> 1
	| Tr(x,l,r) -> num_foglie l + num_foglie r;;

(** ESERCIZIO 5 **)
(* segui_bool: bool list -> 'a tree -> 'a *)
(* segui_bool l t = radice del sottoalbero di T determinato da L, se questo non è vuoto, un errore altrimenti *)
let rec segui_bool l t =
	match (l,t) with
	| ([],Tr(x,_,_)) -> x
	| (_,Empty) -> failwith "errore"
	| (x::rest,Tr(y,left,right)) -> if x then segui_bool rest left
	                                else segui_bool rest right;;

(** ESERCIZIO 6 **)
(* foglia_costo: int tree -> (int * int) *)
(* foglia_costo t = l'etichetta e il costo della foglia piu' costosa dell'albero *)
let foglia_costo t =
	let rec aux sum = function
		| Empty -> failwith "errore"
		| Tr(x,Empty,Empty) -> (x,sum+x)
		| Tr(x,Empty,r) -> (aux (sum+x) r)
		| Tr(x,l,Empty) -> (aux (sum+x) l)
		| Tr(x,l,r) -> let (e1,a) = (aux (sum+x) l) in
		               let (e2,b) = ((aux (sum+x) r)) in
									 if a > b then (e1,a) else (e2,b)
	in aux 0 t;;

(** ESERCIZIO 7 **)
(* foglie_costi: int tree -> (int * int) *)
(* foglie_costi t = lista di coppie, ciascuna delle quali ha la forma (f,n), dove f è l'etichetta di una foglia in T e
n il costo di tale foglia *)
let foglie_costi t =
	let rec aux sum = function
		| Empty -> []
		| Tr(x,Empty,Empty) -> [(x,sum+x)]
		| Tr(x,Empty,r) -> aux (sum+x) r
		| Tr(x,l,Empty) -> aux (sum+x) l
		| Tr(x,l,r) -> (aux (sum+x) l) @ ((aux (sum+x) r))
	in aux 0 t;;

(** ESERCIZIO 8 **)
(* pattern_matching: expr -> expr -> bool *)
(* pattern_matching e m = determini se E e M si confrontano positivamente oppure no *)
let rec pattern_matching e m =
	match (e,m) with
	| (_, Jolly) -> true
  | (Sum(e1,e2),Sum(m1,m2)) | (Diff(e1,e2),Diff(m1,m2)) | (Mult(e1,e2),Mult(m1,m2)) | (Div(e1,e2),Div(m1,m2)) -> pattern_matching e1 m1 && pattern_matching e2 m2
  | (e,m) -> e = m;;

(** ESERCIZIO 9 **)
(* max_common_subtree: string tree -> string tree -> string tree *)
(* max_common_subtree a b = massimo sottoalbero comune a A e B, partendo dalla radice *)
let rec max_common_subtree a b = 
	match (a,b) with
	| (Empty,Empty) -> Empty
	| (Tr(_,_,_),Empty) | (Empty,Tr(_,_,_)) -> Tr("@",Empty,Empty)
	| (Tr(x,l1,r1),Tr(y,l2,r2)) -> if x = y then Tr(x,max_common_subtree l1 l2,max_common_subtree r1 r2)
	                               else Tr("@",Empty,Empty);;

(** ESERCIZIO 10 **)
(* stessa_struttura: 'a tree -> 'a tree -> bool *)
(* stessa_struttura t1 t2 = determini se due alberi binari hanno la stessa struttura *)
let rec stessa_struttura t1 t2 = 
	match (t1,t2) with
	| (Empty,Empty) -> true
	| (Tr(_,l1,r1),Tr(_,l2,r2)) -> stessa_struttura l1 l2 && stessa_struttura r1 r2
	| _ -> false;;

(* esiste_mapping: 'a tree -> 'a tree -> bool *)
(* esiste_mapping t1 t2 = determini se esiste un mapping da t1 a t2 *)
let rec lista_coppie t1 t2 =
	match (t1,t2) with
	| (Empty,Empty) -> []
	| ((Tr(x,l1,r1),Tr(y,l2,r2))) -> let lst = ((lista_coppie l1 l2) @ (lista_coppie r1 r2))
	                                 in if List.mem (x,y) lst then lst
																      else (x,y) :: lst
	| _ -> [];;

let esiste_mapping t1 t2 =
	let doppia_lista = lista_coppie t1 t2
	in let lst = List.map fst doppia_lista
	in (stessa_struttura t1 t2) && (List.for_all (function x -> List.mem x (List.map fst (List.remove_assoc x doppia_lista))) lst);;

(** ESERCIZIO 11 **)
(* path: ('a -> bool) -> 'a tree -> 'a list *)
(* path p t = un cammino dalla radice a una foglia di t che non contenga alcun nodo che soddisfa p *)
let rec path p = function
	| Empty -> failwith "errore"
	| Tr(x,Empty,Empty) -> if p x then  failwith "Errore"
                         else [x]
	| Tr(x,l,r) -> if p x then  failwith "Errore"
                 else x :: (try path p l
                           with Failure "Errore" -> path p r);;

(** ESERCIZIO 12 **)
type 'a sostituzione = ('a * 'a tree) list;;

(* applica: 'a sostituzione -> 'a tree -> 'a tree *)
(* applica subst t = l'albero che si ottiene applicando la sostituzione subst a t *)
let rec applica subst = function
	| Empty -> Empty
	| Tr(x,Empty,Empty) -> (try List.assoc x subst
	                       with _ -> Tr(x,Empty,Empty))
	| Tr(x,l,r) -> Tr(x, applica subst l, applica subst r);;

(** ESERCIZIO 13 **)
(* path_coprente: 'a tree -> 'a list -> 'a list *)
(* path_coprente t l = ramo dell'albero dalla radice a una foglia che contenga tutti i nodi di L (in
qualsiasi ordine) ed eventualmente anche altri nodi *)
let rec path_coprente t l =
	match (t,l) with
	| (Empty,_) -> []
	| (Tr(x,Empty,Empty), lst) -> if lst = [] || lst = [x] then [x]
	                              else failwith "errore"
	| (Tr(x,l,r), y::rest) -> if List.mem x (y::rest) then x:: (try path_coprente l rest
	                                                           with _ -> (try path_coprente r rest
																												                with _ -> failwith "errore"))
														else (try path_coprente l rest
	                                with _ -> (try path_coprente r rest
																						 with _ -> failwith "errore"));;

(** ESERCIZIO 14 **)
type col = Rosso | Giallo | Verde | Blu;;
type 'a col_assoc = (col * 'a list) list;;

(* colore: 'a -> 'a col_assoc -> col *)
(* colore x lst = colore di x, se tale colore è definito *)
let rec colore x = function
	| [] -> failwith "errore"
	| (a,l)::rest -> if List.mem x l then a
	                 else colore x rest;;

type col_option = None | Some of col;;

(* path_to: 'a -> 'a col_assoc -> 'a tree -> 'a list *)
(* path_to x lst t = un ramo a colori alterni, dalla radice dell'albero a una foglia etichettata da x *)
let path_to x lst t = 
	let rec aux prec = function
		| Empty -> failwith "errore"
		| Tr(y,Empty,Empty) -> if y = x then [y]
		                       else failwith "errore"
		| Tr(y,l,r) -> let act = colore y lst in
			             if act <> prec then y :: (try aux act l
									                           with _ -> (try aux act r
																				                with _ -> failwith "errore"))
									 else failwith "errore"
	in aux None t;;

(** ESERCIZIO 15 **)

(* abr_check: ('a * 'b) tree -> bool *)
(* abr_check t = controlla se t e' un albero binario di ricerca *)
let rec abr_check = function
	| Empty | Tr(_,Empty,Empty) -> true
	| Tr((k,_),Tr((k1,_),l1,r1),Tr((k2,_),l2,r2)) -> (k >= k1) && (k <= k2) && (abr_check l1) && (abr_check l2)
	                                                  && (abr_check r1) && (abr_check r2);;

(* abr_search: ('a * 'b) tree -> 'a -> 'b *)
(* abr_search t k = l valore v associato a k nell'albero, un errore altrimenti *)
let rec abr_search t k =
	match t with
	| Empty -> failwith "errore"
	| Tr((c,v),l,r) -> if c = k then v
	                   else if k < c then abr_search l k
                          else abr_search r k;;

(* abr_update: ('a * 'b) tree -> ('a * 'b) -> 'a tree *)
(* abr_update t coppia = l'ABR che si
ottiene da T aggiungendo l'elemento (k, v). Se T già contiene un elemento
con chiave k, il suo valore verrà sostituito con v *)
let rec abr_update t coppia =
	let (k,v) = coppia in 
	match t with
	| Empty -> Tr(coppia,Empty,Empty)
  | Tr((c,m),l,r) -> 	if c = k then Tr((c,v),l,r)
	                    else if k < c then Tr((c,m), abr_update l coppia, r)
                          else Tr((c,m), l, abr_update r coppia);;

(* abr_delmin: 'a tree -> 'a * 'a tree *)
(* abr_delmin t = coppia (label, T0) dove label (una coppia chiave-valore)
è l'etichetta di T con chiave minima e T0 è l'albero che si ottiene da T
eliminando il nodo etichettato da label *)
let rec label = function
	| Empty -> failwith "errore"
	| Tr((k,_),Empty,Empty) -> k
	| Tr((k,_),l,r) -> label l;;

let abr_delmin t =
	let rec aux c = function
		| Empty -> Empty
		| Tr((k,v),l,r) -> if k = c then Empty
		                   else Tr((k,v), aux c l, aux c r)
	in try let min = label t 
	       in (min, aux min t)
		 with _ -> failwith "vuoto";;

(* abr_delete: ('a * 'b) tree -> 'a -> ('a * 'b) tree *)
(* abr_delete t k = l'algoritmo di cancellazione di un elemento da un ABR *)

let rec delroot = function
    Empty -> failwith "delroot"
  | Tr(a,Empty,t2) -> t2
  | Tr(a,t1,Empty) -> t1
  | Tr((k,v),t1,t2) -> let (x,newright) = abr_delmin t2
                   in  Tr((x,v),t1,newright);;

let rec abr_delete abr a = 
  match abr with
    Empty -> Empty
  | Tr((k,_) as b,t1,t2) -> 
       if a = k then delroot (Tr(b,t1,t2))
       else if a < k
            then Tr(b,abr_delete t1 a,t2)
            else Tr(b,t1,abr_delete t2 a);;


(* abr_insert: 'a tree -> 'a -> 'a tree *)
(* abr_insert t n = 'inserimento di un elemento in un ABR *)

let rec abr_insert t x =
	match t with
	| Empty -> Tr(x,Empty,Empty)
  | Tr(y,l,r) -> 	if x <= y
        then Tr(y,abr_insert l x, r)
        else Tr(y,l,abr_insert r x);;



(* tree_sort: 'a list -> 'a list *)
(* tree_sort l = lista l ordinata secondo il tree sort *)
let build_abr l = 
	let rec aux t = function 
	  | [] -> t
	  | x::rest -> aux (abr_insert t x) rest
	in aux Empty l;;

let tree_sort l = inorder (build_abr l);;

