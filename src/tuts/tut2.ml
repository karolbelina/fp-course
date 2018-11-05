(* ex 2 *)
(* calculate the n-th Fibonacci number using regular, as well as tail recursion *)
let rec fibonacciA n = 
	if n < 0 then failwith "negative index"
	else if n = 0 then 0
	else if n = 1 then 1
	else fibonacciA (n - 2) + fibonacciA (n - 1);;

let fibonacciB n = 
	let rec f n a b = 
		if n < 0 then failwith "negative index"
		else if n = 0 then a
		else if n = 1 then b
		else f (n - 1) b (b + a) in
	f n 0 1;;

(* ex 3 *)
(* calculate the cube root of the specified number using the Newton-Raphson method *)
let root3 a = 
	let rec f x = 
		if abs_float (x ** 3. -. a) <= 10e-15 *. abs_float a then x
		else f (x +. (a /. x ** 2. -. x) /. 3.) in
	f (if a > 1. then a /. 3. else a);;

(* ex 5 *)
(* check if the first list is a prefix of the second list *)
let rec initSegment segment list = 
	match (segment, list) with
		([], _) -> true
		| (_, []) -> false
		| _ ->
			if List.hd segment = List.hd list then initSegment (List.tl segment) (List.tl list)
			else false;;

(* ex 6 *)
(* replace the n-th element of a list with the specified value *)
let rec replaceNth xs n x = 
	match xs with
		[] -> []
		| head :: tail ->
			if n = 0 then x :: tail
			else head :: replaceNth tail (n - 1) x;;