(* signature for the excercise 1 *)
module type QUEUE_FUN =
sig
	type 'a t
	exception Empty of string
	val empty: unit -> 'a t
	val enqueue: 'a * 'a t -> 'a t
	val dequeue: 'a t -> 'a t
	val first: 'a t -> 'a
	val isEmpty: 'a t -> bool
end;;

(* ex 1 *)
(* define a module which supports basic operations on a queue represented by a regular list *)
module QueueA : QUEUE_FUN =
struct
	type 'a t = 'a list
	exception Empty of string

	let empty () = []

	let enqueue (e, q) = e :: q

	let rec dequeue = function
		e1 :: e2 :: q -> e1 :: dequeue (e2 :: q)
		| [_] -> []
		| [] -> []

	let rec first = function
		e1 :: e2 :: q -> first (e2 :: q)
		| [e] -> e
		| [] -> raise (Empty "module QueueA: first")

	let isEmpty q = q = []
end;;

(* define a module which supports basic operations on a queue represented by a pair or lists *)
module QueueB : QUEUE_FUN =
struct
	type 'a t = 'a list * 'a list
	exception Empty of string

	let empty () = ([], [])

	let enqueue (e, (xl, yl)) = 
		if xl = [] then (List.rev (e :: yl), [])
		else (xl, e :: yl)

	let dequeue = function
		([], _) -> ([], [])
		| (_ :: tx, yl) -> 
			if tx = [] then (List.rev yl, [])
			else (tx, yl)

	let first = function
		(h :: _, _) -> h
		| ([], _) -> raise (Empty "module QueueB: first")

	let isEmpty (xl, _) = xl = []
end;;

(* tests *)
let q = let open QueueB in enqueue (3, enqueue (2, enqueue (1, empty ())));;
QueueB.first q;;
let open QueueB in first (dequeue (dequeue q));;