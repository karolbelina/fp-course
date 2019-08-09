(* ex 1 *)
(* calculate the sum of all integers in a list *)
let rec sum l = 
  match l with
    [] -> 0
    | x :: tail -> x + (sum tail);;

(* ex 2 *)
(* concatenate the list with a separator in-between each element *)
let concatenate separator x = 
  let rec f a = 
    match a with
      [] -> ""
      | [x] -> x
      | x :: tail -> x ^ separator ^ (f tail) in
  f x;;

(* ex 4 *)
(* check if the three numbers are in a descending order *)
(* kinda weird exercise, but sure *)
let descending a b c = 
  a > b && b > c;;