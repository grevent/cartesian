
open CartesianDataModel
open ListReference
open Env

exception CycleInGenericTypesFound;;

let rec typeGen = ref 0;;
let newGeneric() = 
	typeGen := !typeGen + 1;
	T_GENERIC !typeGen
;;

let rec recursiveReduceGenerics env alreadyTested tp = 
	match tp with
		T_GENERIC id0 when (List.exists (fun x -> x == id0) alreadyTested) -> (id0::alreadyTested,T_GENERIC id0) |
		T_GENERIC id0 -> 
			(try 
				(recursiveReduceGenerics env (id0::alreadyTested) (List.assoc id0 (returnList env.genericTypes)))
			with
				Not_found -> (id0::alreadyTested,T_GENERIC id0) ) |
		T_LIST tp -> 
			let (newAlreadyTested,newType) = (recursiveReduceGenerics env alreadyTested tp) in
			(newAlreadyTested,T_LIST newType) |
		T_ARRAY tp -> 
			let (newAlreadyTested,newType) = (recursiveReduceGenerics env alreadyTested tp) in
			(newAlreadyTested, T_ARRAY newType) |
		T_PAIR tps -> 
			let (newAlreadyTested,newTypes) = List.fold_left (fun (currentAlreadyTested,currentResult) currentType -> 
				let (nextAlreadyTested,newPartialResult) = recursiveReduceGenerics env currentAlreadyTested currentType in
				(nextAlreadyTested,currentResult@[newPartialResult]) ) (alreadyTested,[]) tps 
			in
			(newAlreadyTested,T_PAIR newTypes) |
		T_FUNCTION (param,result) -> 
			let (alreadyTested1,newParamType) = recursiveReduceGenerics env alreadyTested param in
			let (alreadyTested2,newResultType) = recursiveReduceGenerics env alreadyTested1 result in
			(alreadyTested2,T_FUNCTION (newParamType,newResultType)) |
		T_INT -> 
			(alreadyTested,T_INT) |
		T_FLOAT -> 
			(alreadyTested,T_FLOAT) |
		T_STRING -> 
			(alreadyTested,T_STRING) |
		T_BOOL -> 
			(alreadyTested,T_BOOL) |
		T_ACTION -> 
			(alreadyTested,T_ACTION) |
		T_NOD -> 
			(alreadyTested,T_NOD) |
		T_OBJECT ->
			(alreadyTested,T_OBJECT) |
		T_TRANSITION -> 
			(alreadyTested,T_TRANSITION) |
		T_NAMED (id,params) -> 
			let (newAlreadyTested,newTypes) = ListTools.mapWithState (recursiveReduceGenerics env) alreadyTested params in
			(newAlreadyTested,T_NAMED (id,newTypes)) |
		T_VARIANT variants -> 
			let (newAlreadyTested,newVariants) = ListTools.mapWithState (fun currentAlreadyTested (id,currentType) -> 
				let (nextAlreadyTested,newPartialResult) = recursiveReduceGenerics env currentAlreadyTested currentType in 
				(nextAlreadyTested,(id,newPartialResult))) alreadyTested variants 
			in
			(newAlreadyTested,T_VARIANT newVariants)
;;

let reduceGenerics env tp = 
let (_,result) = recursiveReduceGenerics env [] tp in
	result
;;

exception TypesNotCompatible;;
let rec unification env tp1 tp2 = 
	match (tp1,tp2) with
		((T_FUNCTION (param1,result1)),(T_FUNCTION (param2,result2))) -> 
			let param = unification env param1 param2 in
			let result = unification env result1 result2 in 
			T_FUNCTION (param,result) |
		(T_INT,T_INT) -> T_INT |
		(T_FLOAT,T_FLOAT) -> T_FLOAT |
		(T_STRING,T_STRING) -> T_STRING |
		(T_BOOL,T_BOOL) -> T_BOOL |
		(T_NOD,T_NOD) -> T_NOD |
		(T_ACTION,T_ACTION) -> T_ACTION |
		(T_GENERIC id),tp -> 
			addGeneric env id tp;
			tp |
		tp,(T_GENERIC id) -> 
			addGeneric env id tp;
			tp |
		(T_LIST tp1),(T_LIST tp2) -> 
			let tp = unification env tp1 tp2 in 
			T_LIST tp |
		(T_ARRAY tp1),(T_ARRAY tp2) ->  
			let tp = unification env tp1 tp2 in 
			T_ARRAY tp |
		(T_PAIR tps1),(T_PAIR tps2) -> 
			let tps = List.fold_left (fun tps (tp1,tp2) -> let tp = unification env tp1 tp2 in (tps@[tp])) [] (List.combine tps1 tps2) in
			(T_PAIR tps) |
		(T_NAMED (st1,params1)),(T_NAMED (st2,params2)) when (String.compare st1 st2) == 0 -> 
			let tps = List.fold_left (fun tps (tp1,tp2) -> let tp = unification env tp1 tp2 in (tps@[tp])) [] (List.combine params1 params2) in
			(T_NAMED (st1,tps)) |
		_ -> 
			raise TypesNotCompatible
;;

let rec verifyUniqueness env typeLst = 
	match typeLst with
		[] -> 
			true |
		tp::otherTypes -> 
			if (verifyUniqueness env otherTypes) then	
				List.fold_left 
					(fun acc ot -> 
						try ignore (unification env ot tp); false with
							TypesNotCompatible -> acc)
					true
					otherTypes
			else
				false
;;

exception TypeNotAPair
let getTypesFromPair tp = 
	match tp with
		T_PAIR lst -> lst |
		_ -> raise TypeNotAPair
;;

exception TypeNotAGeneric
let getIdFromGeneric tp = 
	match tp with
		T_GENERIC id -> id |
		_ -> raise TypeNotAGeneric
;; 

let rec recursiveRenameGenerics alreadyRenamed renameFn tp = 
	match tp with 
		T_INT -> (alreadyRenamed,T_INT) |
		T_FLOAT -> (alreadyRenamed,T_FLOAT) |
		T_STRING -> (alreadyRenamed,T_STRING) |
		T_BOOL -> (alreadyRenamed,T_BOOL) |
		T_ACTION -> (alreadyRenamed,T_ACTION) | 
		T_GENERIC id -> 
			(try (alreadyRenamed,(List.assoc id alreadyRenamed)) with
			Not_found -> 
				let newType = renameFn id in
				(((id,newType)::alreadyRenamed),newType)) |
		T_NOD -> (alreadyRenamed,T_NOD) |
		T_LIST tp -> 
			let (nextAlreadyRenamed,newType) = recursiveRenameGenerics alreadyRenamed renameFn tp in
			(nextAlreadyRenamed,T_LIST newType) |
		T_ARRAY tp -> 
			let (nextAlreadyRenamed,newType) = recursiveRenameGenerics alreadyRenamed renameFn tp in
			(nextAlreadyRenamed,T_ARRAY newType) |
		T_PAIR tps -> 
			let (nextAlreadyRenamed,newTypes) = List.fold_left (fun (currentAlreadyRenamed,currentTypes) tp -> 
				let (newAlreadyRenamed,newType) = recursiveRenameGenerics currentAlreadyRenamed renameFn tp in
				(newAlreadyRenamed,currentTypes@[newType]) ) (alreadyRenamed,[]) tps
			in
			(nextAlreadyRenamed,T_PAIR newTypes) |
		T_NAMED (id,params) -> 
			let (newList,types) = ListTools.mapWithState (fun lst param -> recursiveRenameGenerics lst renameFn param) alreadyRenamed params in
			(newList,T_NAMED (id,types)) |
		T_VARIANT variants -> 
			let (lst,preVariants) = 
				List.fold_left (fun (alreadyRenamed,renamedVariants) (id,tp) -> 
					let (newAlreadyRenamed,newType) = recursiveRenameGenerics alreadyRenamed renameFn tp in 
					(newAlreadyRenamed,renamedVariants@[(id,newType)]) ) 
					(alreadyRenamed,[]) 
					variants 
			in
			(lst,T_VARIANT preVariants) |
		T_FUNCTION (paramType,resultType) -> 
			let (renamed,newParamType) = recursiveRenameGenerics alreadyRenamed renameFn paramType in
			let (renamed,newResultType) = recursiveRenameGenerics renamed renameFn resultType in
			(renamed,T_FUNCTION (newParamType,newResultType)) |
		T_OBJECT -> (alreadyRenamed,T_OBJECT) |
		T_TRANSITION -> (alreadyRenamed,T_TRANSITION)
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

let testUnification env tp1 tp2 =
	unification env (renameGenerics tp1) (renameGenerics tp2)
;;

exception SubtypeUnificationOnlyWithNamedTypes 
let subtypeUnification env originalType narrowedType  = 
	try 
		unification env originalType narrowedType 
	with
		TypesNotCompatible -> 
			(match narrowedType with
				T_NAMED (id,concreteParams) -> 
					let (tp,genericParameters) = getNamedType env id in
					let typeCorrespondings = ListTools.map2 (fun x y -> (x,y)) genericParameters concreteParams in					
					let rewrittenType = rewriteNamedType typeCorrespondings tp in
					unification env originalType rewrittenType |
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
		T_VARIANT variants -> helper variants |
		_ -> raise TypeNotVariant
;;

exception NoTypesForVariant 
let getTypeForVariant env id = 
	let namedTypes = returnList env.namedTypes in
	let rec helper lst = 
		match lst with
			(nm,tp,params)::cdr -> (try ((T_NAMED (nm,params)),getVariantType tp id) with IdNotInVariant -> helper cdr) |
			[] -> raise NoTypesForVariant
	in
	helper namedTypes
;;
