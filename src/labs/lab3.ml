(* ex 1 *)
(* filter out lists which contain the specified element *)
let filterLists xs x = 
	let rec contains xs = 
		match xs with
			[] -> false
			| h :: t ->
				if h <> x then contains t
				else true in
	let rec f xs acc = 
		match xs with
			[] -> List.rev acc
			| h :: t -> f t (if not (contains h) then h :: acc else acc) in
	f xs [];;