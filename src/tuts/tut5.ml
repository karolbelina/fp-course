(* definition of a lazy list for excercises 1 and 2 *)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

(* auxiliary functions *)
let rec toLazyList = function
    [] -> LNil
    | h :: t -> LCons(h, function () -> toLazyList t);;

let rec ltake = function
    (0, _) -> []
    | (_, LNil) -> []
    | (n, LCons(x, xf)) -> x :: ltake(n - 1, xf ());;

let rec lfrom k = LCons(k, function () -> lfrom (k + 1));;

(* ex 1 *)
(* repeat elements k times in a lazy list *)
let rec lrepeat k = 
    let rec f n x xf = 
        if n > 0 then LCons(x, function () -> f (n - 1) x xf)
        else if n = 0 then lrepeat k (xf ())
        else failwith "negative repetition count"
    in function
        LNil -> LNil
        | LCons(x, xf) -> f k x xf;;

ltake (8, lrepeat 3 (toLazyList [1; 2; 3]));;
ltake (12, lrepeat 4 (lfrom 0));;
ltake (10, lrepeat 2 (toLazyList [0; 4; 1; 3; 2]));;

(* ex 2 *)
(* define a fibonacci sequence using lazy lists *)
let lfib = 
	let rec f a b = LCons(a, function () -> f b (a + b))
	in f 0 1;;

(* definition of a lazy binary tree for excercise 3 *)
type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

(* ex 3 *)
(* create a lazy list containg every element of the potentially infinite lazy binary tree *)
let lBTtoLlist tree =
    let rec f queue = 
        match queue with
            [] -> LNil
            | LEmpty :: t -> f t
            | LNode(v, lf, rf) :: t -> LCons(v, function () -> f (t @ [lf (); rf ()]))
    in f [tree];;

(* construct an infinite lazy binary tree with a root of value n and subtrees of value 2*n and 2*n+1 *)
let rec lTree n = LNode(n, (function () -> lTree (2 * n)), (function () -> lTree (2 * n + 1)));;