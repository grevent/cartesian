
open CartesianDataModel

exception CycleInGenericTypesFound;;

let rec typeGen = ref 0;;
let newGeneric() = 
	typeGen := !typeGen + 1;
	GENTYPE !typeGen
;;

let rec reduceGeneric runtime alreadyResolved id0 =
	if List.exist (fun x -> x == id0) alreadyResolved then
		GENTYPE id
	else
		try match (List.assoc id0 runtime.genericTypes) with
			GENTYPE otherId -> reduceGeneric (id0::alreadyResolved) runtime otherId |
			x -> x
		with Not_found -> 
			GENTYPE id0
;;

exception NamedTypeDoesNotExist;;
let findNamedType namedTypes id0 =
	try
		let (_,tp) = List.find (fun (id,_) -> (String.compare id id0 == 0)) namedTypes in
		tp
	with Not_found ->
		raise NamedTypeDoesNotExist
;;

let rec reduceGenerics generics alreadyTested tp = 
	match tp with
		GENTYPE id0 -> (reduceGenerics generics (id0::alreadyTested) (reduceGeneric generics id0)) |
		LIST tp -> LIST (reduceGenerics generics alreadyTested tp) |
		ARRAY tp -> ARRAY (reduceGenerics generics alreadyTested tp) |
		PAIR tps -> PAIR (List.map (reduceGenerics generics alreadyTested) tps) |
		FUNCTION (params,result) -> FUNCTION ((List.map (reduceGenerics generics alreadyTested) params),(reduceGenerics generics result)) |
		_ -> tp
;;	

exception TypesNotCompatible;;
let rec unification namedTypes generics tp1 tp2 = 
	match (tp1,tp2) with
		(FUNCTION params1,result1),(FUNCTION params2,result2) -> ERROR |
		INT,INT -> (generics,INT) |
		FLOAT,FLOAT -> (generics,FLOAT) |
		UNKNOWN,tp -> (generics,tp) |
		tp,UNKNOWN -> (generics,tp) |
		STRING,STRING -> (generics,STRING) |
		BOOL,BOOL -> (generics,BOOL) |
		NOD,NOD -> (generics,NOD) |
		ACTION,ACTION -> (generics,ACTION) |
		(GENTYPE id),tp -> ((id,tp)::generics,tp) |
		tp,(GENTYPE id) -> ((id,tp)::generics,tp) |
		(LIST tp1),(LIST tp2) -> let (newGens,tp) = unification namedTypes generics tp1 tp2 in (newGens,LIST tp) |
		(ARRAY tp1),(ARRAY tp2) ->  let (newGens,tp) = unification namedTypes generics tp1 tp2 in (newGens,ARRAY tp) |
		(PAIR tps1),(PAIR tps2) -> 
			let (newGens,tps) = List.fold_left (fun (gens,tps) (tp1,tp2) -> let (newGens,tp) = unification namedTypes gens tp1 tp2 in (newGens,(tps@[tp]))) (generics,[]) (List.combine tps1 tps2) in
			(newGens,(PAIR tps)) |
		(NAMED st1),_ -> 
			findNamedType namedTypes st1 |
		_,(NAMED st2) -> 
			findNamedType namedTypes st2 |
		_ -> 
			raise TypesNotCompatible
;;

let rec verifyUniqueness generics typeLst = 
	match typeLst with
		[] -> 
			true |
		tp::otherTypes -> 
			if (verifyUniqueness generics otherTypes) then	
				List.fold_left 
					(fun acc ot -> 
						try ignore (unification [] generics ot tp); false with
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

let testUnification runtime tp1 tp2 =
	unification runtime (renameGenerics tp1) (renameGenerics tp2)
;;
