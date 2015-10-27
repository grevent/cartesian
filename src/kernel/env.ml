
open Types

let env = { 
}
;;

let genericCounter = ref 0;;
let newGeneric() = 
	genericCounter := !genericCounter + 1;
	GENERIC (!genericCounter)
;;
