
let objects = ref ([] : Object.cObject list);;
(* 
let rules = ref [];;
let declarations = ref [];;
*)

let runtime() = 
	while true do
		Unix.sleep 10;
	done;
;;

let getObjects() =
	!objects
;;
