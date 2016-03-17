

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

let createList i_0 i_n step continue =
	let rec helper i =
		match i with
			x when continue x -> i::(helper (step i)) |
			x -> []
	in
	helper i_0
;;

let rec iterTillFalse fn lst = 
	match lst with
		car::cdr -> 
			if fn car then
				ignore 0
			else
				iterTillFalse fn cdr |
		[] -> 
			raise Not_found
;;

let rec firstWorking fn lst = 
	match lst with 
		car::cdr -> 
			(try
				let result = fn car in
				(car,result)
			with _ -> 
				firstWorking fn cdr) |
		[] -> 
			raise Not_found
;;

let firstWorkingPos fn lst =
	let rec helper c lst = 
		match lst with
			car::cdr -> 
				(try 
					ignore (fn car);
					c
				with 
					_ -> helper (c+1) cdr) |
			[] -> 
				raise Not_found
	in
	helper 0 lst
;;
				
let rec allWorking fn lst = 
	match lst with
		car::cdr -> 
			(try 
				let result = fn car in 
				(result::(allWorking fn cdr));
			with _ -> 
				allWorking fn cdr) |
		[] -> 
			[]
;;

let fold_left_i fn start lst = 
	let acc = ref start in 
	for i = 0 to (List.length lst) - 1 do
		acc := fn !acc i (List.nth lst i);
	done;
	!acc
;;
