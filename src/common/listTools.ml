

let rec assoc fn key0 lst = 
	match lst with
		[] -> 
			raise Not_found |
		(key,value)::cdr -> 
			if (fn key0 key) then
				value
			else 
				(assoc fn key0 cdr)
;;
