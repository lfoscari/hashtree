type ('a, 'b) tree =
    Leaf of 'a list * int
  | Node of ('a, 'b) tree array * int * ('a -> 'b) * ('b -> int)
  
class ['a, 'b] hashTree :
  int ->
  ('a -> 'b) list ->
  ('b -> int) list ->
  int ->
  object
    val mutable root : ('a, 'b) tree
    method counting_search :
      ?node:('a, 'b) tree -> ?count:int -> int -> 'b -> int
    method private create_node : 'a list -> int -> ('a, 'b) tree
    method depth : ('a, 'b) tree -> int
    method feature_maps : ('a -> 'b) list
    method featured_gini : 'a list -> ('b -> int) -> ('a -> 'b) -> float
    method insert : 'a -> unit
    method private pick_feature : 'a list -> ('b -> int) -> int * ('a -> 'b)
    method root : ('a, 'b) tree
    method search : ?node:('a, 'b) tree -> int -> 'b -> 'a list
    method usage : ('a, 'b) tree -> float
    method visit : ('a, 'b) tree -> 'a list
  end