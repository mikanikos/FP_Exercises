type literal = Positive of string | Negative of string
val consist : literal list -> bool
val consistent : literal list list -> bool
val listfrom : 'a -> 'a list -> 'a list
