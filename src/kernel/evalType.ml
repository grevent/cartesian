
open Tree 
open CartesianDataModel
open Env
open Type

exception PairElementsWithSameType;;
exception TypeNotInPair;;
exception IdAlreadyDefined;;

let rec createFunctionType types resultType =
	match types with
		[] -> resultType |
		tp::otherTypes -> T_FUNCTION (tp,(createFunctionType otherTypes resultType))
;;

let rec evalMatchType env (patterns,expr) =
	let env1 = newScope env in
	let patternTypes = List.map (evalPatternType env1) patterns in
	let exprType = evalExprType env1 expr in
	let tp = createFunctionType patternTypes exprType in
	let realResult = reduceGenerics env1 tp in
	realResult
and evalExprType env expr = 
	match expr with
		FUNCTIONCALLEXPR (nd,fn,param) -> 
			let fnType = evalExprType env fn in
			let paramType = evalExprType env param in
			let resultType = newGeneric() in
			let tp = T_FUNCTION(paramType,resultType) in
			let _ = unification env tp fnType in
			let realResult = reduceGenerics env resultType in
			addDecoration env nd realResult;
			realResult |
		LAMBDAEXPR (nd,pattern,expr) ->
			let env1 = newScope env in
			let patternType = evalPatternType env1 pattern in
			let exprType = evalExprType env1 expr in
			let resultType = T_FUNCTION(patternType,exprType) in
			let realResult = reduceGenerics env resultType in
			addDecoration env nd realResult;
			realResult | 
		LETEXPR (nd,assigns,expr) -> 
			let env1 = newScope env in
			let patternTypes = List.map (fun (pattern,expr) -> evalPatternType env1 pattern) assigns in
			List.iter2 
				(fun patternType (_,expr) -> 
					let tmpType = evalExprType env1 expr in
					let _ = unification env1 tmpType patternType in
					ignore 0)
				patternTypes
				assigns; 
			let realResult = evalExprType env1 expr in
			addDecoration env nd realResult;
			realResult |
		TYPEACCESSEXPR (nd,expr,typeNode) -> 
			let exprType = evalExprType env expr in
			let types = getTypesFromPair exprType in
			if not (verifyUniqueness env types) then
				raise PairElementsWithSameType;
			let (_,typeType) = evalTypeType env [] typeNode in
			if not (List.fold_left (fun result expr -> 
				if result then 
					result 
				else 
					begin  
						try ignore (testUnification env expr typeType); true with
							TypesNotCompatible -> false
					end;) false types)
			then
				raise TypeNotInPair;
			addDecoration env nd typeType;
			typeType |
		NARROWTYPEEXPR (nd,expr,typeNode) -> (* typenode should be less general, i.e., is a new defined named type *)
			let exprType = evalExprType env expr in
			let (_,typeType) = evalTypeType env [] typeNode in
			let resultType = subtypeUnification env typeType exprType in
			addDecoration env nd resultType;
			resultType |
		GENERALISETYPEEXPR (nd,expr,typeNode) -> (* typenode should be a more general type, i.e., a float for an expr of realPartOfComplex *)
			let exprType = evalExprType env expr in 
			let (_,typeType) = evalTypeType env [] typeNode in
			let resultType = subtypeUnification env exprType typeType in
			addDecoration env nd resultType;
			resultType |
		INTEXPR (nd,_) -> 
			addDecoration env nd T_INT;
			T_INT |
		FLOATEXPR (nd,_) ->
			addDecoration env nd T_FLOAT;
			T_FLOAT |
		STRINGEXPR (nd,_) -> 
			addDecoration env nd T_STRING;
			T_STRING |
		BOOLEXPR (nd,_) -> 
			addDecoration env nd T_BOOL;
			T_BOOL |
		IDEXPR (nd,name) -> 
			let vl = getIdValue env name in
			addDecoration env nd vl.tp;
			vl.tp |
		ACTIONEXPR (nd,actions) -> 
			List.iter (fun action -> evalActionType env action) actions;
			addDecoration env nd T_ACTION;
			T_ACTION |
		LISTEXPR (nd,exprs) -> 
			let resultType = List.fold_left (fun resultType expr -> (unification env resultType (evalExprType env expr))) (newGeneric()) exprs in
			addDecoration env nd (T_LIST resultType);
			(T_LIST resultType) |
		NODEXPR nd -> 
			addDecoration env nd T_NOD;
			T_NOD |
		PAIREXPR (nd,exprs) ->
			let exprsType = List.map (evalExprType env) exprs in
			addDecoration env nd (T_PAIR exprsType);
			T_PAIR exprsType |
		ARRAYEXPR (nd,exprs) -> 
			let resultType = List.fold_left (fun resultType expr -> (unification env resultType (evalExprType env expr))) (newGeneric()) exprs in
			addDecoration env nd (T_ARRAY resultType);
			(T_ARRAY resultType) |
		OBJEXPR (nd,obj) -> 
			addDecoration env nd T_OBJECT;
			evalObjectType env obj;
			T_OBJECT |
		TRANSITIONEXPR (nd,trans) -> 
			addDecoration env nd T_TRANSITION;
			evalTransitionExpr env trans;
			T_TRANSITION |
		TYPEVERIFICATIONEXPR (nd,expr,tpExpr) -> 
			let exprType = evalExprType env expr in
			let (_,typeType) = evalTypeType env [] tpExpr in
			let resultType = unification env typeType exprType in
			addDecoration env nd resultType;
			resultType |
		NATIVEEXPR (nd,tp,fn) -> 
			addDecoration env nd tp;
			tp |
		VARIANTEXPR (nd,id,vl) -> 	
			let vlType = evalExprType env vl in
			let (variantType,tp) = getTypeForVariant env id in
			let _ = unification env tp vlType in
			addDecoration env nd variantType;
			variantType |
		ERROREXPR nd -> 
			let tmp = newGeneric() in
			addDecoration env nd tmp;
			tmp |
		FUNCTIONEXPR (nd,lambdas) -> 
			let types = List.map (evalExprType env) lambdas in
			let resultType = List.fold_left (unification env) (newGeneric()) types in
			addDecoration env nd resultType;
			resultType
and evalPatternType env pattern =
	match pattern with
		CONSPATTERN (nd,carPattern,cdrPattern) -> 
			let carType = evalPatternType env carPattern in
			let cdrType = evalPatternType env cdrPattern in
			let resultType = unification env (T_LIST carType) cdrType in
			addDecoration env nd resultType;
			resultType | 
		INTPATTERN (nd,_) -> 
			addDecoration env nd T_INT;
			T_INT |
		FLOATPATTERN (nd,_) -> 
			addDecoration env nd T_FLOAT;
			T_FLOAT |
		STRINGPATTERN (nd,_) -> 
			addDecoration env nd T_STRING;
			T_STRING | 
		BOOLPATTERN (nd,_) -> 
			addDecoration env nd T_BOOL;
			T_BOOL |
		LISTPATTERN (nd,lst) -> 
			let types = List.map (evalPatternType env) lst in 
			let singleType = List.fold_left (unification env) (newGeneric()) types in 
			addDecoration env nd (T_LIST singleType);
			(T_LIST singleType) |
		RENAMINGPATTERN (nd,pattern,id) -> 
			let patternType = evalPatternType env pattern in
			let idType = newGeneric() in 
			addId env id nd idType;
			let resultType = unification env idType patternType in
			addDecoration env nd resultType;
			resultType | 
		WILDCARDPATTERN nd -> 
			let resultType = newGeneric() in
			addDecoration env nd resultType;
			resultType |
		PAIRPATTERN (nd,patterns) -> 
			let patternTypes = List.map (evalPatternType env) patterns in
			addDecoration env nd (T_PAIR patternTypes);
			(T_PAIR patternTypes) | 
		IDPATTERN (nd,id) -> 
			(try
				(getIdValueCurrentLevel env id).tp
			with
				IdNotDeclared -> 
					let tp = newGeneric() in
					addId env id nd tp;
					tp) | 
		WHEREPATTERN (nd,pattern,expr) -> 
			let patternType = evalPatternType env pattern in
			let exprType = evalExprType env expr in
			ignore (unification env T_BOOL exprType);
			addDecoration env nd patternType;
			patternType | 
		ARRAYPATTERN (nd,lst) -> 
			let types = List.map (evalPatternType env) lst in 
			let singleType = List.fold_left (unification env) (newGeneric()) types in 
			addDecoration env nd (T_LIST singleType);
			(T_ARRAY singleType) |
		TYPEDPATTERN (nd,pattern,tp) -> 
			let patternType = evalPatternType env pattern in
			let (_,typeType) = evalTypeType env [] tp in
			let resultType = unification env patternType typeType in
			addDecoration env nd resultType;
			resultType |
		VARIANTPATTERN (nd,id,pattern) -> 
			let patternType = evalPatternType env pattern in
			let (fullType,subType) = getTypeForVariant env id in
			let _ = unification env subType patternType in
			addDecoration env nd fullType;
			fullType 
and evalTypeType env genericTypes tp =
	match tp with
		NODTYPE -> 
			(genericTypes,T_NOD) |
		INTTYPE -> 
			(genericTypes,T_INT) |
		FLOATTYPE -> 
			(genericTypes,T_FLOAT) |
		STRINGTYPE -> 
			(genericTypes,T_STRING) |
		ARRAYTYPE tp -> 
			let (_,evaluatedTp) = evalTypeType env [] tp in
			(genericTypes,(T_ARRAY evaluatedTp)) |
		LISTTYPE tp -> 
			let (_,evaluatedTp) = evalTypeType env [] tp in
			(genericTypes,(T_LIST evaluatedTp)) |
		GENTYPE id -> 
			(try
				(genericTypes,(ListTools.assoc (fun x -> (String.compare x id) == 0) genericTypes))
			with
				Not_found -> 
					let resultType = newGeneric() in
					(((id,resultType)::genericTypes),resultType)) |
		PAIRTYPE types -> 
			let (newGenericTypes,types) = ListTools.mapWithState (evalTypeType env) genericTypes types in
			(newGenericTypes,(T_PAIR types)) |
		NAMEDTYPE (id,types) ->
			let (newGenericTypes,types) = ListTools.mapWithState (evalTypeType env) genericTypes types in
			(genericTypes,(T_NAMED (id,types))) |
		VARIANTTYPE variants -> 
			let (newGenericTypes,variantTypes) = ListTools.mapWithState (fun currentState (id,typeDescription) -> let (nextState,tp) = (evalTypeType env currentState typeDescription) in (nextState,(id,tp))) genericTypes variants in 
			(newGenericTypes,(T_VARIANT variantTypes)) |  
		FUNCTIONTYPE (paramTypeExpr,resultTypeExpr) -> 
			let (genericTypes1,tp1) = evalTypeType env genericTypes paramTypeExpr in
			let (genericTypes2,tp2) = evalTypeType env genericTypes1 resultTypeExpr in
			(genericTypes2,(T_FUNCTION (tp1,tp2))) |
		OBJECTTYPE -> 
			(genericTypes,T_OBJECT) |
		TRANSITIONTYPE ->
			(genericTypes,T_TRANSITION)
and evalActionType env action =
	match action with
		IMMEDIATEACTION expr -> 
			let tp = evalExprType env expr in
			let _ = unification env T_ACTION tp in
			ignore 0 | 
		ASSIGNACTION (id,expr) -> 
			ignore (evalExprType env expr); |
		ASSIGNRULEACTION (id,expr) -> 
			let tp = evalExprType env expr in
			let _ = unification env T_TRANSITION tp in
			ignore 0 |
		ASSIGNOBJECTACTION (id,expr) -> 
			let tp = evalExprType env expr in
			let _ = unification env T_OBJECT tp in
			ignore 0 |
		EXPRACTION expr -> 
			let tp = evalExprType env expr in
			let _ = unification env (T_LIST T_ACTION) tp in
			ignore 0 |
		DELETERULEACTION id -> 
			ignore 0 |
		DELETEOBJECTACTION id -> 
			ignore 0 |
		DEFINETYPEACTION (id,paramIds,typeExpr) -> 
			let generics = List.map (fun id -> (id,newGeneric())) paramIds in
			let realTypes = List.map (fun (_,tp) -> tp) generics in
			let (_,tp) = evalTypeType env generics typeExpr in
			addType env id tp realTypes |
		DEFINEACTION (nd,id,expr) -> 
			(try 
				ignore (getIdValueRoot env id).tp;
				raise IdAlreadyDefined
			with 
				IdNotDeclared -> 
					let tp = newGeneric() in 
					addIdToRoot env id nd tp;
					let exprType = evalExprType env expr in
					ignore (unification env tp exprType);); |
		DEFINEEXTERNALACTION (nd,id,typeExpr) -> 
			let (_,tp1) = evalTypeType env [] typeExpr in
			let tp2 = evalPatternType env (IDPATTERN (nd,id)) in
			ignore (unification env tp1 tp2); |
		DEFINEOBJECTACTION (id,expr) -> 
			let tp = evalExprType env expr in
			let _ = unification env T_OBJECT tp in
			ignore 0 |
		DEFINERULEACTION (id,expr) -> 
			let tp = evalExprType env expr in 
			let _ = unification env T_TRANSITION tp in
			ignore 0 
and evalObjectType env obj =
	match obj with
		OBJECT atts -> 
			List.iter (fun (_,expr) -> ignore (evalExprType env expr)) atts |
		TRANSIENTOBJECT atts -> 
			List.iter (fun (_,expr) -> ignore (evalExprType env expr)) atts
and evalTransitionExpr env trans = 
	match trans with
		EXPRTRANS (objs,equivalentPattern) -> 
			let env1 = newScope env in
			List.iter (List.iter (evalObjectAttributePattern env1)) objs;
			List.iter (evalObjectAttributePattern env1) equivalentPattern; 
			ignore 0 |
		ACTIONTRANS (objs,actionExpr) -> 
			let env1 = newScope env in
			List.iter (List.iter (evalObjectAttributePattern env1)) objs;
			let tp = evalExprType env1 actionExpr in
			let _ = unification env1 T_ACTION tp in
			ignore 0 
and evalObjectAttributePattern env att = 
	match att with
		VALUEATTRIBUTEPATTERN (id,pattern) -> 
			let _ = evalPatternType env pattern in
			ignore 0 |
		PRESENTATTRIBUTEPATTERN id -> 
			ignore 0 |
		TYPEATTRIBUTEPATTERN (id,tp) -> 
			let _ = evalTypeType env [] tp in
			ignore 0
;;
