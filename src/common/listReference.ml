
type 'a listReference = {
	mutable concreteList: 'a list;
}
;;

let addElement list el = 
	list.concreteList <- el::(list.concreteList);
;;

let deleteElement list = 
	list.concreteList <- List.tl list.concreteList
;;
	
let returnList list = 
	list.concreteList
;;

let isEmpty list = 
	if List.length list.concreteList > 0 then
		true
	else
		false
;;

let newReference lst = {
	concreteList = lst
}
;;
