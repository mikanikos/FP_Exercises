type expr =
    Jolly
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
val subexpr : expr -> expr -> bool
val subst_in_expr : expr -> string -> expr -> expr
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val reflect : 'a tree -> 'a tree
val fulltree : int -> int tree
val altezza : 'a tree -> int
val balanced : 'a tree -> bool
val preorder : 'a tree -> 'a list
val postorder : 'a tree -> 'a list
val inorder : 'a tree -> 'a list
val take : int -> 'a list -> 'a list
val drop : int -> 'a list -> 'a list
val balpreorder : 'a list -> 'a tree
val balinorder : 'a list -> 'a tree
val foglie_in_lista : 'a list -> 'a tree -> bool
val num_foglie : 'a tree -> int
val segui_bool : bool list -> 'a tree -> 'a
val foglia_costo : int tree -> int * int
val foglie_costi : int tree -> (int * int) list
val pattern_matching : expr -> expr -> bool
val max_common_subtree : string tree -> string tree -> string tree
val stessa_struttura : 'a tree -> 'b tree -> bool
val lista_coppie : 'a tree -> 'b tree -> ('a * 'b) list
val esiste_mapping : 'a tree -> 'b tree -> bool
val path : ('a -> bool) -> 'a tree -> 'a list
type 'a sostituzione = ('a * 'a tree) list
val applica : ('a * 'a tree) list -> 'a tree -> 'a tree
val path_coprente : 'a tree -> 'a list -> 'a list
type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
val colore : 'a -> ('b * 'a list) list -> 'b
type col_option = None | Some of col
val path_to : 'a -> (col_option * 'a list) list -> 'a tree -> 'a list
val abr_check : ('a * 'b) tree -> bool
val abr_search : ('a * 'b) tree -> 'a -> 'b
val abr_update : ('a * 'b) tree -> 'a * 'b -> ('a * 'b) tree
val label : ('a * 'b) tree -> 'a
val abr_delmin : ('a * 'b) tree -> 'a * ('a * 'b) tree
val delroot : ('a * 'b) tree -> ('a * 'b) tree
val abr_delete : ('a * 'b) tree -> 'a -> ('a * 'b) tree
val abr_insert : 'a tree -> 'a -> 'a tree
val build_abr : 'a list -> 'a tree
val tree_sort : 'a list -> 'a list
