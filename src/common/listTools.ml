

let rec assoc fn lst = 
	match lst with
		[] -> 
			raise Not_found |
		(key,value)::cdr -> 
			if (fn key) then
				value
			else 
				(assoc fn cdr)
;;

let rec find fn lst = 
	match lst with
	 [] -> 
		raise Not_found |
	key::cdr -> 
		if (fn key) then
			key
		else
			(find fn cdr)
;;

exception ListsNotOfSameLength
let rec map2 fn lst1 lst2 = 
	match (lst1,lst2) with
		(car1::cdr1,car2::cdr2) -> (fn car1 car2)::(map2 fn cdr1 cdr2) |
		([],[]) -> [] |
		_ -> raise ListsNotOfSameLength

let mapWithState fn state0 lst =
	List.fold_left (fun (state,currentResult) el -> let (nextState,transformedEl) = (fn state el) in (nextState,currentResult@[transformedEl])) ([],[]) lst
;;
