
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
		try
			ListTools.find (fun x -> String.compare x.id id == 0) env.values
		with
			_ -> (match runtime.env0.scope with
				ROOT -> raise IdNotDeclared |
				PARENT parentEnv -> search parentEnv);
	in
	search runtime.env0
;;

let getIdValueCurrentLevel runtime id =
	try 
		ListTools.find (fun x -> String.compare x.id id == 0) runtime.env0.values
	with
		_ -> raise IdNotDeclared
;;
	
let addGeneric runtime id tp =
	addElement runtime.genericTypes (id,tp)
;;

let getGeneric runtime id = 
	List.assoc id (returnList runtime.genericTypes);
;;

let existsGeneric runtime id = 
	List.exists (fun (x,_) -> if x == id then true else false) (returnList runtime.genericTypes)
;;

let addDecoration runtime nd tp = 
	addElement runtime.decorations (nd,tp)
;;

let getDecoration runtime nd = 
	List.assoc nd (returnList runtime.decorations)
;;

let addId runtime id nd tp vl = 
	try 
		let vlForId = getIdValueCurrentLevel runtime id in
		(match vlForId.value with
			TBDEXPR -> vlForId.value <- vl; |
			_ -> runtime.env0.values <- {id= id; nodeId= nd; tp= tp; value= vl }::runtime.env0.values;)
	with 
		IdNotDeclared -> runtime.env0.values <- {id= id; nodeId= nd; tp= tp; value= vl }::runtime.env0.values;
;;

let addIdWithoutValue runtime id nd tp = 
	runtime.env0.values <- { id=id; nodeId=nd; tp= tp; value=TBDEXPR }::runtime.env0.values;
;;

exception NamedTypeNotDefined;;
let getNamedType runtime id = 
	let rec recursiveHelper lst = 
		match lst with
			(name,tp,params)::cdr when (String.compare name id == 0) -> (tp,params) |
			car::cdr -> recursiveHelper cdr |
			[] -> raise NamedTypeNotDefined  
	in
	recursiveHelper (returnList runtime.namedTypes)
;;

let addType runtime id tp params = 
	addElement runtime.namedTypes (id,tp,params)
;;

let copyVal vl = 
	{id= vl.id; nodeId= vl.nodeId; value= vl.value; tp= vl.tp }
;;
	
let rec copyEnv env = 
	match env.scope with
		ROOT -> 
			{ scope= ROOT; values= List.map (fun x -> x) env.values } |
		PARENT parentEnv -> 
			{ scope= PARENT (copyEnv parentEnv); values= List.map (fun x -> x) env.values }
;;  

let copyRuntime runtime = {
	rules = runtime.rules;
	objects = runtime.objects;
	env0 = copyEnv runtime.env0;
	decorations= runtime.decorations;
	genericTypes= runtime.genericTypes;
	namedTypes= runtime.namedTypes;
}
;;
