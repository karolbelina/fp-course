(* ex 1 *)
(* binary search tree functions *)
type int_tree =
    | Empty
    | Node of int * int_tree * int_tree;;

(*
an example tree
      8
   3     10
 1   6     14
    4 7   13
*)
let example_tree =
    Node(8, Node(3, Node(1, Empty, Empty), Node(6, Node(4, Empty, Empty), Node(7, Empty, Empty))), Node(10, Empty, Node(14, Node(13, Empty, Empty), Empty)));;

(* check if a tree contains the specified element *)
let rec contains t x = 
  match t with
    | Empty -> false
    | Node(v, l, r) -> contains l x || v == x || contains r x;;

(* calculate the sum of all elements in a tree *)
let rec sum_tree t = 
  match t with
    | Empty -> 0
    | Node(v, l, r) -> sum_tree l + v + sum_tree r;;

(* calculate the product of all elements in a tree *)
let rec prod_tree t = 
  match t with
    | Empty -> 1
    | Node(v, l, r) -> prod_tree l * v * prod_tree r;;