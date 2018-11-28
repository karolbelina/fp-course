(* ex 1 *)
(* define a module which supports basic operations on a binary search tree *)

(* signature *)
module type TREE =
sig
    type t
    val create: unit -> t
    val push: int -> t -> unit
    val remove: int -> t -> unit
    val find: int -> t -> bool
    val getPreOrder: t -> int list
    val getPostOrder: t -> int list
    val getInOrder: t -> int list
end;;

(* structure *)
module BST : TREE = 
struct
    type tree = 
        Empty
        | Node of int * tree * tree;;
    type t = {mutable root : tree}
    exception DuplicateElement of int

    let create () = {root = Empty}

    let push x t = 
        let rec f = function
            Empty -> Node(x, Empty, Empty)
            | Node(v, l, r) ->
                if x > v then Node(v, l, f r)
                else if x < v then Node(v, f l, r)
                else raise (DuplicateElement x)
        in t.root <- f t.root;;

    let remove x t = 
        let rec deleteMin = function
            Node(v, Empty, r) -> (v, r)
            | Node(v, l, r) ->
                let (v, l2) = deleteMin l in (v, Node(v, l2, r))
            | Empty -> failwith "implementation error"
        in let rec f = function
            Empty -> Empty
            | Node(v, l, r) ->
                if x > v then Node(v, l, f r)
                else if x < v then Node(v, f l, r)
                else match (l, r) with
                    (Empty, r) -> r
                    | (l, Empty) -> l
                    | _ -> let (k, r2) = deleteMin r in Node(k, l, r2)
        in t.root <- f t.root;;

    let find x t = 
        let rec f = function
            Node(v, l, r) ->
                if x > v then f r
                else if x < v then f l
                else true
            | Empty -> false
        in f t.root;;

    let getPreOrder t = 
        let rec f = function
            Node(v, l, r) -> v :: (f l @ f r)
            | Empty -> []
        in f t.root;;

    let getPostOrder t = 
        let rec f = function
            Node(v, l, r) -> f l @ (f r @ [v])
            | Empty -> []
        in f t.root;;

    let rec getInOrder t = 
        let rec f = function
            Node(v, l, r) -> f l @ (v :: f r)
            | Empty -> []
        in f t.root;;
end;;

(* tests *)
let t = BST.create ();;
BST.push 5 t;;
BST.push 3 t;;
BST.push 1 t;;
BST.push 4 t;;
BST.push 7 t;;

BST.getPreOrder t;;
BST.getPostOrder t;;
BST.getInOrder t;;

BST.remove 5 t;;
BST.getPreOrder t;;

BST.find 7 t;;
BST.remove 7 t;;
BST.find 7 t;;