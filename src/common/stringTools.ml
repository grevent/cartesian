
let rec concatAndInsert el lst = 
	match lst with 
		[] -> "" |
		[x] -> x |
		x1::x2::y3 -> x1^el^(concatAndInsert el (x2::y3))
;;
