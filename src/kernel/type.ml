
type cType = 
	UNKNOWN |
	INT |
	FLOAT |
	STRING |
	BOOL |
	ACTION |
	GENTYPE of int |
	NOD |
	LIST of cType |
	ARRAY of cType |
	PAIR of cType list |
	NAMED of string | 
	ALTERNATIVE of cType list
;;

exception CycleInGenericTypesFound;;

let rec typeGen = ref 0;;
let newGeneric() = 
	typeGen := !typeGen + 1;
	GENTYPE !typeGen
;;

let rec reduceGeneric generics id0 =
	try 
		(List.assoc id0 generics)
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
		_ -> tp
;;	

exception TypesNotCompatible;;
exception PairElementsWithSameType;;
let rec unification namedTypes generics tp1 tp2 = 
	match (tp1,tp2) with
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
			if verifyUniqueness newGens tps then 	
				(newGens,(PAIR tps)) 
			else
				raise PairElementsWithSameType |
		(NAMED st1),_ -> 
			findNamedType namedTypes st1 |
		_,(NAMED st2) -> 
			findNamedType namedTypes st2 |
		_ -> 
			raise TypesNotCompatible
and verifyUniqueness generics typeLst = 
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
