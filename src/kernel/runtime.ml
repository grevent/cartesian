
open CartesianDataModel
open ListReference

let newRuntime() = {
	objects = newReference [];
	rules = newReference [];
	env0 = { scope= ROOT; values= [] };
	decorations = newReference [];
	genericTypes = newReference [];
	namedTypes = newReference [];
}
;;

let newScope runtime = {
	rules = runtime.rules;
	objects = runtime.objects;
	env0 = { scope = PARENT runtime.env0; values= [] };
	decorations= runtime.decorations;
	genericTypes= runtime.genericTypes;
	namedTypes= runtime.namedTypes;
}
;;

let getObjects runtime = 
	returnList runtime.objects
;;

exception IdNotDeclared
let getIdValue runtime id =
	let rec search env =
		if List.exists (fun x -> (String.compare x.id id) == 0) env.values then
			List.find (fun x -> (String.compare x.id id) == 0) env.values 
		else
			(match runtime.env0.scope with
				ROOT -> raise IdNotDeclared |
				PARENT parentEnv -> search parentEnv);
	in
	search runtime.env0
;;

let getValueType vl = 
	vl.cType
;;

let getValueValue vl =
	vl.value
;;
	
let addGeneric runtime id tp =
	addElement runtime.genericTypes (id,tp)
;;

exception NamedTypeDoesNotExist;;
let findNamedType runtime id0 =
	try
		let (_,tp) = List.find (fun (id,_) -> (String.compare id id0 == 0)) (returnList runtime.namedTypes) in
		tp
	with Not_found ->
		raise NamedTypeDoesNotExist
;;
		
