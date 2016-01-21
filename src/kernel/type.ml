
open CartesianDataModel
open ListReference
open Runtime

exception CycleInGenericTypesFound;;

let rec typeGen = ref 0;;
let newGeneric() = 
	typeGen := !typeGen + 1;
	GENERIC !typeGen
;;

let rec recursiveReduceGenerics runtime alreadyTested tp = 
	match tp with
		GENERIC id0 when (List.exists (fun x -> x == id0) alreadyTested) -> (id0::alreadyTested,GENERIC id0) |
		GENERIC id0 -> 
			(try 
				(recursiveReduceGenerics runtime (id0::alreadyTested) (List.assoc id0 (returnList runtime.genericTypes)))
			with
				Not_found -> (id0::alreadyTested,GENERIC id0) ) |
		LIST tp -> 
			let (newAlreadyTested,newType) = (recursiveReduceGenerics runtime alreadyTested tp) in
			(newAlreadyTested,LIST newType) |
		ARRAY tp -> 
			let (newAlreadyTested,newType) = (recursiveReduceGenerics runtime alreadyTested tp) in
			(newAlreadyTested, ARRAY newType) |
		PAIR tps -> 
			let (newAlreadyTested,newTypes) = List.fold_left (fun (currentAlreadyTested,currentResult) currentType -> 
				let (nextAlreadyTested,newPartialResult) = recursiveReduceGenerics runtime currentAlreadyTested currentType in
				(nextAlreadyTested,currentResult@[newPartialResult]) ) (alreadyTested,[]) tps 
			in
			(newAlreadyTested,PAIR newTypes) |
		FUNCTION (param,result) -> 
			let (alreadyTested1,newParamType) = recursiveReduceGenerics runtime alreadyTested param in
			let (alreadyTested2,newResultType) = recursiveReduceGenerics runtime alreadyTested1 result in
			(alreadyTested2,FUNCTION (newParamType,newResultType)) |
		INT -> 
			(alreadyTested,INT) |
		FLOAT -> 
			(alreadyTested,FLOAT) |
		STRING -> 
			(alreadyTested,STRING) |
		BOOL -> 
			(alreadyTested,BOOL) |
		ACTION -> 
			(alreadyTested,ACTION) |
		NOD -> 
			(alreadyTested,NOD) |
		OBJECT ->
			(alreadyTested,OBJECT) |
		TRANSITION -> 
			(alreadyTested,TRANSITION) |
		NAMED (id,params) -> 
			let (newAlreadyTested,newTypes) = ListTools.mapWithState (recursiveReduceGenerics runtime) alreadyTested params in
			(newAlreadyTested,NAMED (id,newTypes)) |
		VARIANT variants -> 
			let (newAlreadyTested,newVariants) = ListTools.mapWithState (fun currentAlreadyTested (id,currentType) -> 
				let (nextAlreadyTested,newPartialResult) = recursiveReduceGenerics runtime currentAlreadyTested currentType in 
				(nextAlreadyTested,(id,newPartialResult))) alreadyTested variants 
			in
			(newAlreadyTested,VARIANT newVariants)
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
		(GENERIC id),tp -> 
			addGeneric runtime id tp;
			tp |
		tp,(GENERIC id) -> 
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
		(NAMED (st1,params1)),(NAMED (st2,params2)) when (String.compare st1 st2) == 0 -> 
			let tps = List.fold_left (fun tps (tp1,tp2) -> let tp = unification runtime tp1 tp2 in (tps@[tp])) [] (List.combine params1 params2) in
			(NAMED (st1,tps)) |
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

exception TypeNotAGeneric
let getIdFromGeneric tp = 
	match tp with
		GENERIC id -> id |
		_ -> raise TypeNotAGeneric
;; 
		

let rec recursiveRenameGenerics alreadyRenamed renameFn tp = 
	match tp with 
		INT -> (alreadyRenamed,INT) |
		FLOAT -> (alreadyRenamed,FLOAT) |
		STRING -> (alreadyRenamed,STRING) |
		BOOL -> (alreadyRenamed,BOOL) |
		ACTION -> (alreadyRenamed,ACTION) | 
		GENERIC id -> 
			(try (alreadyRenamed,(List.assoc id alreadyRenamed)) with
			Not_found -> 
				let newType = renameFn id in
				(((id,newType)::alreadyRenamed),newType)) |
		NOD -> (alreadyRenamed,NOD) |
		LIST tp -> 
			let (nextAlreadyRenamed,newType) = recursiveRenameGenerics alreadyRenamed renameFn tp in
			(nextAlreadyRenamed,LIST newType) |
		ARRAY tp -> 
			let (nextAlreadyRenamed,newType) = recursiveRenameGenerics alreadyRenamed renameFn tp in
			(nextAlreadyRenamed,ARRAY newType) |
		PAIR tps -> 
			let (nextAlreadyRenamed,newTypes) = List.fold_left (fun (currentAlreadyRenamed,currentTypes) tp -> 
				let (newAlreadyRenamed,newType) = recursiveRenameGenerics currentAlreadyRenamed renameFn tp in
				(newAlreadyRenamed,currentTypes@[newType]) ) (alreadyRenamed,[]) tps
			in
			(nextAlreadyRenamed,PAIR newTypes) |
		NAMED (id,params) -> 
			let (newList,types) = ListTools.mapWithState (fun lst param -> recursiveRenameGenerics lst renameFn param) alreadyRenamed params in
			(newList,NAMED (id,types)) |
		VARIANT variants -> 
			let (lst,preVariants) = 
				List.fold_left (fun (alreadyRenamed,renamedVariants) (id,tp) -> 
					let (newAlreadyRenamed,newType) = recursiveRenameGenerics alreadyRenamed renameFn tp in 
					(newAlreadyRenamed,renamedVariants@[(id,newType)]) ) 
					(alreadyRenamed,[]) 
					variants 
			in
			(lst,VARIANT preVariants) |
		FUNCTION (paramType,resultType) -> 
			let (renamed,newParamType) = recursiveRenameGenerics alreadyRenamed renameFn paramType in
			let (renamed,newResultType) = recursiveRenameGenerics renamed renameFn resultType in
			(renamed,FUNCTION (newParamType,newResultType)) |
		OBJECT -> (alreadyRenamed,OBJECT) |
		TRANSITION -> (alreadyRenamed,TRANSITION)
;;
					
let renameGenerics tp = 
	let (_,result) = recursiveRenameGenerics [] (fun id -> newGeneric()) tp in
	result
;;

let rewriteNamedType genericInstances tp = 
	let (_,result) = recursiveRenameGenerics 
		[] 
		(fun id -> try ListTools.assoc (fun y -> if ((getIdFromGeneric y) == id) then true else false) genericInstances with Not_found -> newGeneric())
		tp
	in
	result
;;

let testUnification runtime tp1 tp2 =
	unification runtime (renameGenerics tp1) (renameGenerics tp2)
;;

exception SubtypeUnificationOnlyWithNamedTypes 
let subtypeUnification runtime originalType narrowedType  = 
	try 
		unification runtime originalType narrowedType 
	with
		TypesNotCompatible -> 
			(match narrowedType with
				NAMED (id,concreteParams) -> 
					let (tp,genericParameters) = getNamedType runtime id in
					let typeCorrespondings = ListTools.map2 (fun x y -> (x,y)) genericParameters concreteParams in					
					let rewrittenType = rewriteNamedType typeCorrespondings tp in
					unification runtime originalType rewrittenType |
				_ -> 
					raise SubtypeUnificationOnlyWithNamedTypes)
;;

exception TypeNotVariant
exception IdNotInVariant
let getVariantType tp id0 = 
	let rec helper variants = 
		match variants with
			(id,tp)::_ when String.compare id id0 == 0 -> tp |
			_::cdr -> helper cdr |
			[] -> raise IdNotInVariant
	in
	match tp with 
		VARIANT variants -> helper variants |
		_ -> raise TypeNotVariant
;;

exception NoTypesForVariant 
let getTypeForVariant runtime id = 
	let namedTypes = returnList runtime.namedTypes in
	let rec helper lst = 
		match lst with
			(nm,tp,params)::cdr -> (try ((NAMED (nm,params)),getVariantType tp id) with IdNotInVariant -> helper cdr) |
			[] -> raise NoTypesForVariant
	in
	helper namedTypes
;;
