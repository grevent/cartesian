
let objects = ref ([] : Object.cObject list);;
let env = ref ([]: Env.env list)
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

let getEnv() = 
	!env
;;
