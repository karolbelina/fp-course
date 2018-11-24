(* definition of a lazy list *)
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