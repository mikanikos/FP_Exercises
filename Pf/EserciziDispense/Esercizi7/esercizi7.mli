type direzione = Su | Giu | Destra | Sinistra
type posizione = int * int * direzione
type azione2 = Gira | Avanti of int
val gira : direzione -> direzione
val avanti : int * int * direzione -> int -> int * int * direzione
val sposta : int * int * direzione -> azione2 -> int * int * direzione
val esegui : int * int * direzione -> azione2 list -> int * int * direzione
type nat = Zero | Succ of nat
val somma : nat -> nat -> nat
val int_of_nat : nat -> int
val prodotto : nat -> nat -> nat
type chiave = Aperta | Chiusa
type cassaforte = chiave list
val giraPrima : chiave list -> chiave list
val giraDopoChiusa : chiave list -> chiave list
val successori : chiave list -> chiave list list
type obj = Miss | Cann | Barca
type situazione = obj list * obj list
val initial : obj list * 'a list
type azione = From_left of obj list | From_right of obj list
val conta : 'a -> 'a list -> int
val safe : obj list * obj list -> bool
exception Fail
val verifica_errori : obj list -> obj list -> bool
val togli_un : 'a -> 'a list -> 'a list
val togli : 'a list -> 'a list -> 'a list
val applica : azione -> obj list * obj list -> obj list * obj list
val actions : azione list
val from_sit : obj list * obj list -> (obj list * obj list) list
type 'a pattern = Jolly | Val of 'a
val most_general_match : 'a list -> 'a list -> 'a pattern list
