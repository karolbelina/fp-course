(* ex 1 *)
(* deduce the type of the following functions *)

(* let f1 x = x 2 2;; *)
(* x : int -> int -> 'a = <fun> *)
(* val f1 : (int -> int -> 'a) -> 'a = <fun> *)

(* let f2 x y z = x ( y ^ z );; *)
(* y : string *)
(* z : string *)
(* x : string -> 'a = <fun> *)
(* val f2 : (string -> 'a) -> string -> string -> 'a = <fun> *)

(* ex 2 *)
(* define functions curry3 and uncurry3 *)
let curry3 f x y z = f (x, y, z);;
(* let curry3' = function f -> function x -> function y -> function z -> f (x, y, z);; *)
(* val curry3 : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd = <fun> *)

let uncurry3 f (x, y, z) = f x y z;;
(* let uncurry3' = function f -> function (x, y, z) -> f x y z;; *)
(* val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd = <fun> *)

(* ex 3 *)
(* define a specific function using List.fold_left *)
let sumProd xs = List.fold_left (fun (sum, prod) x -> (sum + x, prod * x)) (0, 1) xs;;

(* ex 5 *)
(* insertion sort *)
let insertionsort pred xs = 
    let rec insert x = function
        [] -> [x]
        | h :: t as xs -> if (pred h x)
            then h :: insert x t
            else x :: xs
    in List.fold_left (fun acc x -> insert x acc) [] xs;;

(* usage *)
insertionsort (fun x y -> x < y) [5; 3; 8; 6; 2; 9; 7; 6; 1];;

(* merge sort *)
let mergesort pred xs = 
    let split xs = 
        let rec f (xs, ys) = function
            [] -> (List.rev xs, List.rev ys)
            | [x] -> (List.rev (x :: xs), List.rev ys)
            | x :: y :: t -> f (x :: xs, y :: ys) t
        in f ([], []) xs
    in let rec merge = function
        [], ys -> ys
        | xs, [] -> xs
        | hx :: tx as xs, (hy :: ty as ys) -> if (pred hx hy)
            then hx :: merge (tx, ys)
            else hy :: merge (ty, xs)
    in let rec f = function
        [] -> []
        | [x] -> [x]
        | _ as xs -> let (left, right) = split xs in merge (f left, f right)
    in f xs;;

(* usage *)
mergesort (fun x y -> x < y) [5; 3; 8; 6; 2; 9; 7; 6; 1];;