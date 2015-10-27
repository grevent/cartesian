
type objectAtRuntime = { obj: CartesianObject.cartesianObject; locked: Mutex.t; mutable changed: bool }

let objects = ref ([] : objectAtRuntime list);;
(* 
let rules = ref [];;
let declarations = ref [];;
*)

let runtime() = 
	while true do
		Unix.sleep 10;
	done;
;;
