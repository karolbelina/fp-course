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
(* insert an element to an already sorted list *)
let rec insert x = function
    [] -> [x]
    | h :: t as xs -> if h < x
        then h :: insert x t
        else x :: xs;;

insert 4 [1; 3; 5; 7];;
insert 1 [];;
insert 100 [2; 4; 6; 20; 40];;

(* ex 2 *)
(* duplicate elements based on their value in a regular/lazy list *)
let rec duplicate = 
    let rec f n x xs = 
        if n > 0 then x :: f (n - 1) x xs
        else if n = 0 then duplicate xs
        else failwith "negative integer"
    in function
        [] -> []
        | h :: t -> f h h t;;

duplicate [1; 2; 3];;
duplicate [0; 4; 1; 3; 2];;
duplicate [2; 0];;

let rec lduplicate = 
    let rec f n x xf = 
        if n > 0 then LCons(x, function () -> f (n - 1) x xf)
        else if n = 0 then lduplicate (xf ())
        else failwith "negative integer"
    in function
        LNil -> LNil
        | LCons(x, xf) -> f x x xf;;

ltake (6, lduplicate (toLazyList [1; 2; 3]));;
ltake (12, lduplicate (lfrom 0));;
ltake (12, lduplicate (toLazyList [0; 4; 1; 3; 2]));;

(* ex 3 *)
(* some dog bullshit *)
type dog = Dog of string * (unit -> string);;

let azor = Dog("blablador", function () -> "samolot");;
let reks = Dog("kundel", function () -> "rower");;
let pies = Dog("kot", function () -> "trebusz");;
let burek = Dog("jamnik", function () -> "wózek inwalidzki");;

let getBreed (Dog (b, v)) = b;;
let getVehicle (Dog (b, v)) = v ();;