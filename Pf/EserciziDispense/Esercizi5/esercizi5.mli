val in_labirinto : int -> int * int -> bool
val filter_vicini : int -> (int * int) list -> (int * int) list
val find_content : ('a * 'b list) list -> 'b -> 'a list
val raccolti : ('a * 'b list) list -> 'b list -> 'a list
val combine : 'a list -> 'b list -> ('a * 'b) list
val split : ('a * 'b) list -> 'a list * 'b list
val cancella : 'a -> ('a * 'b) list -> ('a * 'b) list
val union : 'a list -> 'a list -> 'a list
val diff : 'a list -> 'a list -> 'a list
val inters : 'a list -> 'a list -> 'a list
val subset : 'a list -> 'a list -> bool
val explode : string -> char list
val implode : char list -> string
val sugg : 'a -> 'b list -> ('a * 'b) list
val upto : int -> int list
val intpairs : int -> (int * int) list
val trips : 'a list -> ('a * 'a * 'a) list
val take : int -> 'a list -> 'a list
val choose : int -> 'a list -> 'a list list
val strike_ball : 'a list -> 'a list -> int * int
val insert : 'a -> 'a list -> 'a list
val insertion_sort : 'a list -> 'a list
val partition : 'a list -> 'a -> 'a list * 'a list
val quick_sort : 'a list -> 'a list
