(* definition of a regular and a lazy list *)
type 'a nlist = Nil | Cons of 'a * ('a nlist);;
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

(* auxiliary functions *)
let rec toNList = function
    [] -> Nil
    | h :: t -> Cons(h, toNList t);;

let rec toLazyList = function
    [] -> LNil
    | h :: t -> LCons(h, function () -> toLazyList t);;

let rec toList = function
    Nil -> []
    | Cons(x, xs) -> x :: toList xs;;

let rec ltake = function
    (0, _) -> []
    | (_, LNil) -> []
    | (n, LCons(x, xf)) -> x :: ltake(n - 1, xf ());;

let rec lfilter pred = function
    LNil -> LNil
    | LCons(x, xf) -> if pred x
        then LCons(x, function () -> lfilter pred (xf ()))
        else lfilter pred (xf ());;

let rec lfrom k = LCons(k, function () -> lfrom (k + 1));;

let primes =
    let rec sieve = function
        LCons(p,nf) -> LCons(p, function () -> sieve (lfilter (function n -> n mod p <> 0) (nf())))
        | LNil -> failwith "Impossible! Internal error."
    in sieve (lfrom 2);;

(* ex 1 *)
(* divide a lazy list into two separate lazy lists with alternating elements *)
let rec divide = function
    Nil -> (Nil, Nil)
    | Cons(x, Nil) -> (Cons(x, Nil), Nil)
    | Cons(x, Cons(y, xs)) ->
        let (even, odd) = divide xs in (Cons(x, even), Cons(y, odd));;

let ldivide = 
    let rec alternate = function
        LNil -> LNil
        | LCons(x, xf) -> match xf () with
            LNil -> LCons(x, function () -> LNil)
            | LCons(_, yf) -> LCons(x, function () -> alternate (yf ()))
    in function
        LNil -> (LNil, LNil)
        | LCons(_, xf) as x -> (alternate x, alternate (xf ()));;

(* tests *)
let (even, odd) = divide Nil in (toList even, toList odd);;
let (even, odd) = divide (toNList [5; 6; 3; 2; 1]) in (toList even, toList odd);;
let (even, odd) = divide (toNList [1; 2; 3; 4; 5; 6; 7; 8]) in (toList even, toList odd);;

let (even, odd) = ldivide (toLazyList [5; 6; 3; 2; 1]) in (ltake(6, even), ltake(6, odd));;
let (even, odd) = ldivide (lfrom 0) in (ltake(6, even), ltake(6, odd));;
let (even, odd) = ldivide primes in (ltake(6, even), ltake(6, odd));;