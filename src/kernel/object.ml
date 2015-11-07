
let objectCounter = ref 0

type objectEntry = { name: string; mutable value: Tree.exprNode; locked: Mutex.t}
type cObject = { id: int; attributes: objectEntry list; mutable changed: bool; interface: string };;

let objectToString obj =
	let tmp = List.fold_left (fun result att -> result^att.name^"= "^(Tree.exprToString att.value)^"; ") "{ " obj.attributes in
	tmp^"} "
;; 

let objectToStringRepresentation obj = 
	(Printf.sprintf "id: %d \t| %s" obj.id (objectToString obj))
;;

