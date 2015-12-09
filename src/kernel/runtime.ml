
open CartesianDataModel

let newRuntime() = 
	{
		objects= [];
		rules= [];
		env0= { scope= ROOT; values= [] };
		decorations= [];
		genericTypes= [];
		namedTypes= [];
	}
;;

let getObjects runtime = 
	runtime.objects
;;

exception IdNotDeclared
let getIdValue runtime id =
	let rec search env =
		if List.exists (fun x -> (String.compare x.id id) == 0) env.values then
			List.find (fun x -> (String.compare x.id id) == 0) env.values 
		else
			(match env.parentScope with
				ROOT -> raise IdNotDeclared |
				PARENT parentEnv -> search parentEnv);
	in
	search runtime.env0
;;

let getValueType vl = 
	vl.cType
;;

let getValueValue vl =
	value.value
;;
