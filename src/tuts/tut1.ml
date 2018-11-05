(* ex 1 *)
(* flatten the list of lists by one level *)
let rec flatten xss = 
	if xss = [] then []
	else List.hd xss @ flatten (List.tl xss);;

(* ex 2 *)
(* count the occurences of an element in a list *)
let rec count x xs = 
	if xs = [] then 0
	else if List.hd xs = x then count x (List.tl xs) + 1 else count x (List.tl xs);;

(* ex 3 *)
(* return a list in which the same element is repeated n number of times *)
let rec replicate x n = 
	if n < 0 then failwith "ujemny argument"
	else if n = 0 then []
	else x :: replicate x (n - 1);;

(* ex 4 *)
(* square each element of a list *)
let rec sqrList xs = 
	if xs = [] then []
	else List.hd xs * List.hd xs :: sqrList (List.tl xs);;

(* ex 5 *)
(* check if the specified list is a palindrome *)
let palindrome xs = 
	let len = List.length xs in
	let rec f n = 
		if n = 0 then true
		else if List.nth xs (len - n) = List.nth xs (n - 1) then f (n - 1)
		else false in
	f (len / 2);;

(* ex 6 *)
(* calculate the length of a list *)
let rec listLength xs = 
	if xs = [] then 0
	else listLength (List.tl xs) + 1;;