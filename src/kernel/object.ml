
open CartesianDataModel

let objectCounter = ref 0

let objectToString obj =
	let tmp = List.fold_left (fun result att -> result^att.name^"= "^(Tree.exprToString att.value)^"; ") "{ " obj.attributes in
	tmp^"} "
;; 

let objectToStringRepresentation (obj: cObject) = 
(Printf.sprintf "id: %d \t| %s" obj.id (objectToString obj))
;;
