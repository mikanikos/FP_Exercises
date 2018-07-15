type 'a graph = ('a * 'a) list
val successori : 'a -> ('a * 'b) list -> 'b list
val vicini : 'a -> ('a * 'a) list -> 'a list
val test_connessi : ('a * 'a) list -> 'a -> 'a -> bool
val esiste_ciclo : ('a * 'a) list -> 'a -> bool
val ciclo : ('a * 'a) list -> 'a -> 'a list
type 'a graph2 = 'a list * ('a * 'a) list
val is_connected : ('a * 'a) list -> 'a -> 'a -> bool
val grafo_connesso : 'a list * ('a * 'a) list -> bool
type obj = Miss | Cann | Barca
type situazione = obj list * obj list
val initial : obj list * 'a list
type azione = From_left of obj list | From_right of obj list
val conta : 'a -> 'a list -> int
val safe : obj list * obj list -> bool
exception Impossible
val togli_un : 'a -> 'a list -> 'a list
val togli : 'a list -> 'a list -> 'a list
val applica : azione -> obj list * obj list -> obj list * obj list
val actions : azione list
val from_sit : obj list * obj list -> (obj list * obj list) list
val goal : 'a list * 'b -> bool
val equal : 'a list * 'b list -> 'a list * 'b list -> bool
val miss_cann : (obj list * obj list) list
val from_sit2 : obj list * obj list -> (azione * (obj list * obj list)) list
val miss_cann2 : azione list
type 'a graph3 = 'a list * ('a * 'a) list
val elimina : 'a -> 'a list -> 'a list
val cammino : 'a * ('b * 'b) list -> 'b list -> 'b -> 'b -> 'b list
val hamiltoniano : 'a list * ('a * 'a) list -> 'a list
type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
val colore : 'a -> ('b * 'a list) list -> 'b
val successori_diversi :
  ('a * 'b list) list -> 'b -> ('b * 'b) list -> 'b list
val colori_alterni :
  ('a * 'a) list -> ('b * 'a list) list -> 'a -> 'a -> 'a list
val connessi_in_glist : ('a * 'a) list list -> 'a -> 'a -> bool
val cammino_con_nodi : ('a * 'a) list -> 'a -> 'a list -> 'a list
type chiave = Aperta | Chiusa
type cassaforte = chiave list
exception Fail
val gira : chiave -> chiave
val giraPrima : chiave list -> chiave list
val giraDopoChiusa : chiave list -> chiave list
val veryHardSucc : chiave list -> chiave list list
val cons : 'a -> 'a list -> 'a list
val tutte_liste_con : int -> 'a -> 'a -> 'a list list
val nodi : int -> chiave list list
val archi : int -> (chiave list * chiave list list) list
val start : int -> chiave list
val aperta : chiave list -> bool
val apri : int -> chiave list list
val primo : int -> bool
val cammino_di_primi : (int * int) list -> int -> int -> int list
type form =
    Prop of string
  | Not of form
  | And of form * form
  | Or of form * form
val iscontradictory : form list -> form -> bool
val non_contradictory_path : (form * form) list -> form -> form -> form list
val path_n_p : ('a * 'a) list -> ('a -> bool) -> 'b -> int -> 'a -> 'a list
