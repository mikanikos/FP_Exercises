type 'a ntree = Tr of 'a * 'a ntree list
type multi_expr =
    MultiInt of int
  | MultiVar of string
  | MultiDiff of multi_expr * multi_expr
  | MultiDiv of multi_expr * multi_expr
  | MultiSum of multi_expr list
  | MultiMult of multi_expr list
val subexpr : multi_expr -> multi_expr -> bool
val subst : multi_expr -> string -> multi_expr -> multi_expr
val postorder : 'a ntree -> 'a list
val postlist : 'a -> 'a ntree list -> 'a list
val inorder : 'a ntree -> 'a list
val foglie_in_lista : 'a list -> 'a ntree -> bool
val sumof : int list -> int
val num_di_foglie : 'a ntree -> int
val maxList : ('a * 'b) list -> 'a * 'b
val foglia_costo : int ntree -> int * int
val tutte_foglie_costi : int ntree -> (int * int) list
exception Notfound
val remove : 'a -> 'a list -> 'a list
val ramo_da_lista : 'a ntree -> 'a list -> 'a -> 'a list
val auxlist : 'a list -> 'a -> 'a ntree list -> 'a list
val primo : int -> bool
val ramo_di_primi : int ntree -> int
val auxprimi : int ntree list -> int
val path_non_pred : ('a -> bool) -> 'a ntree -> 'a list
val auxpath : ('a -> bool) -> 'a ntree list -> 'a list
val same_structure : 'a ntree -> 'b ntree -> bool
type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
val colore : 'a -> ('b * 'a list) list -> 'b
val ramo_colorato : col -> (col * col list) list -> col ntree -> col list
