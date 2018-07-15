type 'a graph = ('a * 'a) list;;

let successori n g =
	List.map snd (List.filter (function (x,y) -> x=n) g);;

let rec vicini n = function
	| [] -> []
	| (x,y)::rest -> if x = n then y :: vicini n rest
	                 else if y = n then x :: vicini n rest
								        else vicini n rest;;

(** ESERCIZIO 1 **)
(* test_connessi: 'a graph -> 'a -> 'a -> bool *)
(* test_connessi g m n = determini se esiste un cammino da n a m *)
let test_connessi g n m = 
	let rec aux vis = function
		| [] -> false
		| x::rest -> if List.mem x vis then aux vis rest
		             else m=x || aux (n::vis) ((successori n g) @ rest)
	in aux [] [n];;


(** ESERCIZIO 2 **)
(* esiste_ciclo: 'a graph -> 'a -> bool *)
(* esiste_ciclo: se esiste un ciclo su N (cioè un cammino da N a N che contenga almeno un arco) *)
let esiste_ciclo g n =
	let rec aux vis = function
		| [] -> false
		| x::rest -> if List.mem x vis then x=n || aux vis rest
		             else aux (n::vis) ((successori n g) @ rest)
	in aux [] [n];;

(** ESERCIZIO 3 **)
(* ciclo: 'a graph -> 'a -> 'a list *)
(* ciclo g n = un ciclo su N, altrimenti sollevi un'eccezione *)
let rec ciclo g n =
	let rec aux visited = function
      [] -> failwith "errore"
    | x::rest -> if List.mem x visited then aux visited rest
                 else if x = n then [n]
                      else try x:: aux (x::visited) (successori x g)
                           with _ -> aux visited rest
  in aux [] (successori n g);;

(** ESERCIZIO 4 **)
type 'a graph2 = ('a list * (('a * 'a) list));;

(* grafo_connesso: 'a graph -> bool *)
(* grafo_connesso g = determina se G è connesso *)

(* is_connected: 'a graph -> 'a -> 'a -> bool *)
(* is_connected g m n = determini se esiste un cammino da n a m in g non orientato *)
let is_connected g n m = 
	let rec aux vis = function
		| [] -> false
		| x::rest -> if List.mem x vis then aux vis rest
		             else m=x || aux (n::vis) ((vicini n g) @ rest)
	in aux [] [n];;

let rec grafo_connesso g =
	let rec aux m = function
		| [] -> true
	  | x::rest -> is_connected (snd g) m x && aux m rest
	in match fst g with
	  | [] -> true
		| x::rest -> aux x rest;;

(** ESERCIZIO 5 **)
type obj = Miss | Cann | Barca;;

type situazione = obj list * obj list;;
let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], []);;
type azione =
    From_left of obj list
  | From_right of obj list;;

let conta x lst =
  List.length (List.filter ((=) x) lst);;

let safe (left,right) =
  let aux riva =
    let miss = conta Miss riva
    in miss=0 || miss >= conta Cann riva
  in aux left && aux right;;

exception Impossible;;

let rec togli_un x = function
    [] -> raise Impossible
  | y::rest -> 
      if y=x then rest
      else y::togli_un x rest;;

let rec togli source = function
    [] -> source
  | x::rest ->
      togli (togli_un x source) rest;;

let applica act (left,right) =
  let result = 
    match act with
      From_left lst ->
  if List.length lst > 2 || lst=[]
  then raise Impossible
  else (togli_un Barca (togli left lst),
        Barca::lst @ right)
    | From_right lst ->
  if List.length lst > 2 || lst=[]
  then raise Impossible
  else (Barca::lst @ left,
        togli_un Barca (togli right lst))
  in if safe result then result
  else raise Impossible;;

let actions =
  let elems =
    [[Miss];[Cann];[Miss;Cann];[Miss;Miss];[Cann;Cann]]
  in (List.map (function x -> From_left x) elems)
  @ (List.map (function x -> From_right x) elems);;

let from_sit sit =
  let rec aux = function
      [] -> []
    | a::rest ->
  try applica a sit :: aux rest
  with Impossible -> aux rest
  in aux actions;;

(* goal: situazione -> bool *)
(* goal s = se la situazione e' l'obiettivo *)
let goal (sinistra,_) =
  sinistra = [];;

(* miss_cann: unit -> situazione *)
(* miss_cann = lista delle situazioni rappresentante una possibile soluzione al problema *)
let equal (sin,dx) (sinistra,destra) =
  List.sort compare sin = List.sort compare sinistra && List.sort compare dx = List.sort compare destra;;

let rec miss_cann =
	let rec aux visited = function
      [] -> failwith "errore"
    | x::rest -> if List.exists (equal x) visited then aux visited rest
                 else if goal x then [x]
                      else try x:: aux (x::visited) (from_sit x)
                           with _-> aux visited rest
  in aux [] [initial];;

(* from_sit2 : situazione -> (azione * situazione) list *)
let from_sit2 sit =
  let rec aux = function
      [] -> []
    | a::rest -> try (a, applica a sit) :: aux rest
		             with _ -> aux rest
  in aux actions;;

let rec miss_cann2 =
	let rec aux visited = function
      [] -> failwith "errore"
    | (a,x)::rest -> if List.exists (equal x) visited then aux visited rest
                     else if goal x then [a]
                          else try a:: aux (x::visited) (from_sit2 x)
                               with _-> aux visited rest
  in let ini = initial in
  if goal ini then []
  else aux [ini] (from_sit2 ini);;

(** ESERCIZIO 6 **)
type 'a graph3 = 'a list * ('a * 'a) list;;

(* cammino: 'a graph -> 'a list -> 'a -> 'a -> 'a list*)
(* cammino g l n m = se esiste, un cammino da n a m che passi solo per nodi contenuti in L e per ciascuno di essi esattamente una volta *)
let rec elimina m = function
	| [] -> []
	| x::rest -> if x=m then elimina m rest
	             else x::elimina m rest;;

let rec cammino g l n m =
	let rec aux l = function
		| [] -> failwith "errore"
		| x::rest -> if not(List.mem x l) then failwith "errore"
		             else if x=m && l=[x] then [x]
                       else try x::aux (elimina x l) (successori x (snd g))
											      with _-> aux (elimina x l) rest
  in aux l [n];; 

(* hamiltoniano: 'a graph -> 'a list *)
(* gamiltoniano g = se in G esiste un ciclo hamiltoniano e riporti un tale ciclo, se esiste, un errore altrimenti *)
let hamiltoniano g =
	if (fst g) = [] then failwith "errore"
	else let n = List.hd (fst g) in
	     cammino g (fst g) n n;;

(** ESERCIZIO 7 **)
type col = Rosso | Giallo | Verde | Blu;;
type 'a col_assoc = (col * 'a list) list;;

(* colori_alterni: 'a graph -> 'a col_assoc -> 'a -> 'a -> 'a list *)
(* colori_alterni g un cammino a colori alterni da start a goal, nel grafo dato *)	
let rec colore x = function
    [] -> failwith "colore"
  | (c,list)::rest -> 
      if List.mem x list then c
      else colore x rest;;

let successori_diversi colori nodo grafo =
  let col = colore nodo colori in
  List.filter 
    (function x -> colore x colori <> col)
    (successori nodo grafo)

let colori_alterni g colori m n =
	let rec aux vis x =
		if List.mem x vis then failwith "errore"
		else if x = n then [x]
		     else x :: aux_list (x::vis) (successori_diversi colori x g)
	and aux_list vis = function
		| [] -> failwith "errore"
		| x::rest -> try aux vis x
		             with _-> aux_list vis rest
	in aux [] m;;

(** ESERCIZIO 8 **)
(* connessi_in_glist: 'a graph list -> 'a -> 'a -> bool *)
(* connessi_in_glist l b c = se b e c sono nodi diversi tali che, per qualche grafo Gi
appartenente alla lista [G1,G2,...,Gn], esiste un cammino da b a c o
viceversa (da c a b) *)
let connessi_in_glist l b c = b <> c && List.exists (fun x -> (test_connessi x b c || test_connessi x c b)) l;;

(** ESERCIZIO 9 **)
(* cammino_con_nodi: 'a graph -> 'a -> 'a list -> 'a list *)
(* cammino_con_nodi g n l = un cammino senza cicli che, partendo
da N, contenga tutti i nodi di L (in qualsiasi ordine) ed eventualmente
anche altri nodi *)
let cammino_con_nodi g n l =
	let rec aux_search vis list a =
		if List.mem a vis then failwith "ERRORE"
		else if l = [] || l = [a] then [a]
		     else a :: aux_search_list (a::vis) (List.filter ((<>) a) list) (successori a g)
	and aux_search_list vis list = function
		| [] -> failwith "errore"
		| x::rest -> try aux_search vis list x
		             with _-> aux_search_list vis list rest
  in aux_search [] l n;;

(** ESERCIZIO 10 **)
type chiave = Aperta | Chiusa;;

type cassaforte = chiave list;;

exception Fail;;

(* gira: chiave -> chiave, gira una chiave *)
let gira = function
    Aperta -> Chiusa
  | Chiusa -> Aperta;;

(* giraPrima: cassaforte -> cassaforte, riporta la configurazione
che si ottiene girando la prima chiave *)
let giraPrima = function
    x::rest -> (gira x)::rest
  | _ -> raise Fail;;

(*giraDopoChiusa: cassaforte -> cassaforte,  riporta la configurazione
che si ottiene girando la chiave che segue la prima chiusa. 
Fallisce se non Ã¨ possibile eseguire lâ€™operazione *)
let rec giraDopoChiusa = function
    Chiusa::x::rest ->
      Chiusa::(gira x)::rest
  | Aperta::rest ->
      Aperta::(giraDopoChiusa rest)
  | _ -> raise Fail;;

(* veryHardSucc: cassaforte -> cassaforte list *)
(* la funzione chiamata successori nell'es 3 del gruppo 7 *)
let veryHardSucc config =
  (giraPrima config)::
  (try [giraDopoChiusa config]
  with Fail -> []);;

(* nodi: int -> cassaforte list *)
(* nodi n = la lista con tutte le possibili configurazioni
di una cassaforte con N chiavi *)
let cons x rest = x::rest;;

let rec tutte_liste_con n x y =
	if n = 0 then [[]]
	else let tmp = tutte_liste_con (n-1) x y
       in (List.map (cons x) tmp) @ (List.map (cons y) tmp);;

let nodi n = tutte_liste_con n Aperta Chiusa;;

(* archi: int -> (cassaforte * cassaforte list) list *)
(* archi n = lista con tutte le coppie (clist,successori
clist), per ogni possibile configurazione clist di una cassaforte con
N chiavi *)
let archi n = 
	let comb = nodi n
  in List.map (fun x -> (x, veryHardSucc x)) comb;; 

(* start: int -> cassaforte *)
(* start n = la configurazione iniziale di una cassaforte con N chiavi, in cui tutte sono chiuse *)
let rec start n = 
	if n = 0 then []
	else Chiusa :: (start (n-1));;

(* aperta: cassaforte -> bool *)
(* aperta cass = determina se tutte le chiavi della cassaforte sono aperte *)
let aperta cass = List.for_all (fun x -> x = Aperta) cass;;

(* apri: int -> cassaforte list *)
(* apri n = ricerca di un cammino nel grafo a partire dalla configurazione
iniziale in cui tutte le N chiavi sono chiuse (start N), fino alla
configurazione in cui le N chiavi sono tutte aperte *)
let apri n =
	let rec aux vis a =
		if List.mem a vis then failwith "errore"
		else if aperta a then [a]
		     else a :: aux_list (a::vis) (veryHardSucc a)
	and aux_list vis = function
		| [] -> failwith "errore"
		| x::rest -> try aux vis x
		             with _ -> aux_list vis rest
	in aux [] (start n);;

(** ESERCIZIO 11 **)
(* cammino_di_primi: int graph -> int-> int -> int list *)
(* cammino_di_primi g start goal = cammino in g da start a goal costituito soltanto da numeri primi *)
let primo n =
  let rec aux = function
      1 -> true
    | k -> (n mod k)<>0 && aux (k-1)
  in n=1 || aux (n/2);;

let cammino_di_primi g start goal =
  let rec from_node visited a =
    if List.mem a visited || not (primo a) then failwith "errore"
    else if a = goal then [a]
         else a::from_list (a::visited) (successori a g)
  and from_list visited = function
      [] -> failwith "errore"
    | a::rest -> try from_node visited a 
                 with _ -> from_list visited rest
   in from_node [] start;;

(** ESERCIZIO 12 **)
type form = Prop of string | Not of form | And of form * form | Or of form * form;;

(* non_contradictory_path: form graph -> form -> form -> form list *)
(* non_contradictory_path g start goal = un cammino non contraddittorio da start a goal nel grafo dato *)
let iscontradictory l = function
	| Not f -> List.mem (Not(Not f)) l || List.mem f l
  | f -> List.mem (Not f) l;;

let non_contradictory_path g start goal =
  let rec from_node visited a =
    if List.mem a visited || not (iscontradictory visited a) then failwith "errore"
    else if a = goal then [a]
         else a::from_list (a::visited) (successori a g)
  and from_list visited = function
      [] -> failwith "errore"
    | a::rest -> try from_node visited a 
                 with _ -> from_list visited rest
   in from_node [] start;;

(** ESERCIZIO 13 **)
(* path_n_p: 'a graph -> ('a -> bool) -> int -> 'a -> 'a list *)
(* path_n_p g p n start = un cammino non ciclico da start fino a un
nodo x che soddisfa p e che contenga esattamente n nodi che soddisfano
p (incluso x) *)
let path_n_p g p n start =
  let rec from_node visited m a =
    if List.mem a visited then failwith "errore"
    else if m=1 && p a then [a]
         else a::from_list (a::visited) (if p a then m-1 else m) (successori a g)
  and from_list visited m = function
      [] -> failwith "errore"
    | a::rest -> try from_node visited (m-1) a 
                 with _ -> from_list visited (m-1) rest
   in from_node [] start;;
