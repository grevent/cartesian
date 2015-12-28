
open CartesianDataModel
open ListReference
open Runtime

exception CycleInGenericTypesFound;;

let rec typeGen = ref 0;;
let newGeneric() = 
	typeGen := !typeGen + 1;
	GENTYPE !typeGen
;;

let rec recursiveReduceGenerics runtime alreadyTested tp = 
	match tp with
		GENTYPE id0 when (List.exists (fun x -> x == id0) alreadyTested) -> GENTYPE id0 |
		GENTYPE id0 -> 
			(try 
				reduceGenerics runtime (id0::alreadyTested) (List.assoc id0 (returnList runtime.genericTypes))
			with
				Not_found -> GENTYPE id0) |
		LIST tp -> LIST (reduceGenerics runtime alreadyTested tp) |
		ARRAY tp -> ARRAY (reduceGenerics runtime alreadyTested tp) |
		PAIR tps -> PAIR (List.map (reduceGenerics runtime alreadyTested) tps) |
		FUNCTION (param,result) -> FUNCTION ((reduceGenerics runtime alreadyTested param),(reduceGenerics runtime alreadyTested result)) |
		_ -> tp
;;

let reduceGenerics runtime tp = 
	let (_,result) = recursiveReduceGenerics runtime [] tp in
	result
;;

exception TypesNotCompatible;;
let rec unification runtime tp1 tp2 = 
	match (tp1,tp2) with
		((FUNCTION (param1,result1)),(FUNCTION (param2,result2))) -> 
			let param = unification runtime param1 param2 in
			let result = unification runtime result1 result2 in 
			FUNCTION (param,result) |
		(INT,INT) -> INT |
		(FLOAT,FLOAT) -> FLOAT |
		(STRING,STRING) -> STRING |
		(BOOL,BOOL) -> BOOL |
		(NOD,NOD) -> NOD |
		(ACTION,ACTION) -> ACTION |
		(GENTYPE id),tp -> 
			addGeneric runtime id tp;
			tp |
		tp,(GENTYPE id) -> 
			addGeneric runtime id tp;
			tp |
		(LIST tp1),(LIST tp2) -> 
			let tp = unification runtime tp1 tp2 in 
			LIST tp |
		(ARRAY tp1),(ARRAY tp2) ->  
			let tp = unification runtime tp1 tp2 in 
			ARRAY tp |
		(PAIR tps1),(PAIR tps2) -> 
			let tps = List.fold_left (fun tps (tp1,tp2) -> let tp = unification runtime tp1 tp2 in (tps@[tp])) [] (List.combine tps1 tps2) in
			(PAIR tps) |
		(NAMED st1),_ -> 
			findNamedType runtime st1 |
		_,(NAMED st2) -> 
			findNamedType runtime st2 |
		_ -> 
			raise TypesNotCompatible
;;

let rec verifyUniqueness runtime typeLst = 
	match typeLst with
		[] -> 
			true |
		tp::otherTypes -> 
			if (verifyUniqueness runtime otherTypes) then	
				List.fold_left 
					(fun acc ot -> 
						try ignore (unification runtime ot tp); false with
							TypesNotCompatible -> acc)
					true
					otherTypes
			else
				false
;;

exception TypeNotAPair
let getTypesFromPair tp = 
	match tp with
		PAIR lst -> lst |
		_ -> raise TypeNotAPair
;;

let recursiveRenameGenerics alreadyRenamed tp = 
	match tp with 
		INT -> (alreadyRenamed,INT) |
		FLOAT -> (alreadyRenamed,FLOAT) |
		STRING -> (alreadyRenamed,STRING) |
		BOOL -> (alreadyRenamed,BOOL) |
		ACTION -> (alreadyRenamed,ACTION) | 
		GENTYPE id -> 
			try (alreadyRenamed,List.assoc (fun id0 -> id == id0) alreadyRenamed) with
			Not_found -> let genericType = newGeneric() in 
				(((id,genericType)::alreadyRenamed),genericType) |
		NOD -> (alreadyRenamed,NOD) |
		LIST tp -> (alreadyRenames,(recursiveRenameGenerics alreadyRenamed tp)) |
		ARRAY tp -> (alreadyRenames,(recursiveRenameGenerics alreadyRenamed tp)) |
		PAIR tps -> List.fold_left (fun (lst,tpList) tp -> let (resultList,resultType) = recursiveRenameGenerics lst tp in (resultList,tpList@[resultType])) alreadyRenamed tps |
		NAMED (id,params) -> 
			let (newList,types) = List.fold_left (fun (lst,tpList) param -> let (resultList,resultType) = recursiveRenameGenerics lst param in (resultList,tpList@[resultType])) params in
			(newList,NAMED (id,types)) |
		VARIANT variants -> List.fold_left (fun (lst,variants) (id,tp) -> let (resultList,resultType) = recursiveRenameGenerics lst tp in (
		
		
		
let renameGenerics runtime tp = 
	let (_,result) = recursiveRenameGenerics runtime [] tp in
	result
;;

	VARIANT of (string*cType) list |
	FUNCTION of cType*cType |
	OBJECT |
	TRANSITION |
	INCHANNEL | 
	OUTCHANNEL 
;;


let testUnification runtime tp1 tp2 =
	unification runtime (renameGenerics runtime [] tp1) (renameGenerics runtime [] tp2)
;;

