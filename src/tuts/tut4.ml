(* ex 1 *)
(* deduce the type of the following functions *)

(* let f1 x y z = x y z;; *)
(* y : 'a *)
(* z : 'b *)
(* x : 'a -> 'b -> 'c = <fun> *)
(* f1 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c = <fun> *)

(* let f2 x y = function z -> x::y;; *)
(* x : 'a *)
(* y : 'a list *)
(* z : 'b *)
(* f2 : 'a -> 'a list -> 'b -> 'a list = <fun> *)

(* ex 2 *)
(* define any function f : 'a -> 'b *)
let rec f x = f x;;
let f' x = raise Exit;;

(* definition of a binary tree for excercises 3 and 4 *)
type 'a bt = 
    Empty
    | Node of 'a * 'a bt * 'a bt;;

(* an example tree *)
let tt = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty));;

(* ex 3 *)
(* return a list of nodes in order they were visited doing a breadth-first search on a tree *)
let breadthBT tree =
    let rec f queue = 
        match queue with
            [] -> []
            | Empty :: t -> f t
            | Node(v, l, r) :: t -> v :: f (t @ [l; r])
    in f [tree];;

(* ex 4 *)
(* return the internal and external path length of an extended binary tree *)
let internalPathLength tree = 
    let rec f depth = function
        Empty -> 0
        | Node(_, l, r) -> depth + f (depth + 1) l + f (depth + 1) r
    in f 0 tree;;

let externalPathLength tree = 
    let rec f depth = function
        Empty -> depth
        | Node(_, l, r) -> f (depth + 1) l + f (depth + 1) r
    in f 0 tree;;

(* definition of a graph for excercise 5 *)
type 'a graph = 
    Graph of ('a -> 'a list);;

(* an example graph *)
let g = Graph(function
    0 -> [3]
    | 1 -> [0;2;4]
    | 2 -> [1]
    | 3 -> []
    | 4 -> [0;2]
    | n -> failwith ("Graph g: node " ^ string_of_int n ^ " doesn't exist")
);;

(* ex 5 *)
(* return a list of nodes in order they were visited doing a depth-first search on a graph *)
let depthSearch (Graph graph) node =
    let rec f stack visited =
        match stack with
            [] -> []
            | h :: t -> if List.mem h visited
                then f t visited
                else h :: f (graph h @ t) (h :: visited)
    in f [node] [];;