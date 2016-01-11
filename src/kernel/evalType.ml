
open Tree 
open CartesianDataModel
open Runtime
open Type

exception PairElementsWithSameType;;
exception TypeNotInPair;;

let rec createFunctionType types resultType =
	match types with
		[] -> resultType |
		tp::otherTypes -> FUNCTION (tp,(createFunctionType otherTypes resultType))
;;

let rec evalAssign runtime (pattern,patterns,expr) = 
	if List.length patterns > 0 then
		begin
			let runtime1 = newScope runtime in
			let patternTypes = List.map (evalPatternType runtime1) patterns in 
			let exprType = evalExprType runtime1 expr in
			let tp1 = createFunctionType patternTypes exprType in  
			let realTp1 = reduceGenerics runtime tp1 in
			let tp2 = evalPatternType runtime1 pattern in
			ignore (unification runtime1 realTp1 tp2);
		end
	else
		begin
			let exprType = evalExprType runtime expr in
			let tp = evalPatternType runtime pattern in
			ignore (unification runtime exprType tp);
		end;
and evalMatch runtime (patterns,expr) =
	let runtime1 = newScope runtime in
	let patternTypes = List.map (evalPatternType runtime1) patterns in
	let exprType = evalExprType runtime1 expr in
	let tp = createFunctionType patternTypes exprType in
	let realResult = reduceGenerics runtime1 tp in
	realResult
and evalExprType runtime expr = 
	match expr with
		FUNCTIONCALLEXPR (nd,fn,params) -> 
			let fnType = evalExprType runtime fn in
			let paramsType = List.map (evalExprType runtime) params in
			let resultType = newGeneric() in
			let tp = createFunctionType paramsType resultType in
			let _ = unification runtime tp fnType in
			let realResult = reduceGenerics runtime resultType in
			addDecoration runtime nd realResult;
			realResult |
		LAMBDAEXPR (nd,functions) -> 
			let resultType = newGeneric() in 
			let matchsType = List.map (evalMatch runtime) functions in			 
			let _ = List.fold_left (unification runtime) resultType matchsType in
			let realResult = reduceGenerics runtime resultType in
			addDecoration runtime nd realResult;
			realResult | 
		LETEXPR (nd,assigns,expr) -> 
			let runtime1 = newScope runtime in
			let _ = List.map (fun (pattern,patterns,expr) -> evalPatternType runtime1 pattern) assigns in
			List.iter (evalAssign runtime1) assigns; 
			let realResult = evalExprType runtime1 expr in
			addDecoration runtime nd realResult;
			realResult |
		MATCHEXPR (nd,expr,assigns) -> 
			let exprType = evalExprType runtime expr in
			let matchsType = List.map (evalMatch runtime)  assigns in
			let resultType = newGeneric() in
			let _ = List.fold_left(unification runtime) (FUNCTION (exprType,resultType)) matchsType in
			let realResult = reduceGenerics runtime resultType in
			addDecoration runtime nd realResult;
			realResult |
		MATCHPOSSIBLEEXPR (nd,expr,assigns) -> 
			let exprListType = evalExprType runtime expr in
			let exprType = newGeneric() in
			ignore (unification runtime (LIST exprType) exprListType);
			let matchsType = List.map (evalMatch runtime)  assigns in
			let resultType = newGeneric() in
			let _ = List.fold_left (unification runtime) (FUNCTION (exprType,resultType)) matchsType in
			let realResult = reduceGenerics runtime resultType in
			addDecoration runtime nd realResult;
			realResult |
		TYPEACCESSEXPR (nd,expr,typeNode) -> 
			let exprType = evalExprType runtime expr in
			let types = getTypesFromPair exprType in
			if not (verifyUniqueness runtime types) then
				raise PairElementsWithSameType;
			let (_,typeType) = evalTypeType runtime [] typeNode in
			if not (List.fold_left (fun result expr -> 
				if result then 
					result 
				else 
					begin  
						try ignore (testUnification runtime expr typeType); true with
							TypesNotCompatible -> false
					end;) false types)
			then
				raise TypeNotInPair;
			addDecoration runtime nd typeType;
			typeType |
		NARROWTYPEEXPR (nd,expr,typeNode) -> (* typenode should be less general, i.e., is a new defined named type *)
			let exprType = evalExprType runtime expr in
			let (_,typeType) = evalTypeType runtime [] typeNode in
			let resultType = subtypeUnification runtime typeType exprType in
			addDecoration runtime nd resultType;
			resultType |
		GENERALISETYPEEXPR (nd,expr,typeNode) -> (* typenode should be a more general type, i.e., a float for an expr of realPartOfComplex *)
			let exprType = evalExprType runtime expr in 
			let (_,typeType) = evalTypeType runtime [] typeNode in
			let resultType = subtypeUnification runtime exprType typeType in
			addDecoration runtime nd resultType;
			resultType |
		INTEXPR _ -> 
			INT |
		FLOATEXPR _ -> 
			FLOAT |
		STRINGEXPR _ -> 
			STRING |
		BOOLEXPR _ -> 
			BOOL |
		IDEXPR (nd,name) -> 
			let vl = getIdValue runtime name in
			addDecoration runtime nd (getValueType vl);
			(getValueType vl) |
		ACTIONEXPR (nd,actions) -> 
			List.iter (fun action -> evalActionType runtime action) actions;
			addDecoration runtime nd ACTION;
			ACTION |
		LISTEXPR (nd,exprs) -> 
			let resultType = List.fold_left (fun resultType expr -> (unification runtime resultType (evalExprType runtime expr))) (newGeneric()) exprs in
			addDecoration runtime nd (LIST resultType);
			(LIST resultType) |
		INTERVALEXPR (expr1,expr2) ->
			let tp1 = evalExprType runtime expr1 in
			let tp2 = evalExprType runtime expr2 in
			ignore (unification runtime tp1 INT);
			ignore (unification runtime tp2 INT);
			(LIST INT) |
		INTERVALSTEPEXPR (expr1,expr2,expr3) -> 
			let tp1 = evalExprType runtime expr1 in
			let tp2 = evalExprType runtime expr2 in
			let tp3 = evalExprType runtime expr3 in
			ignore (unification runtime tp1 INT);
			ignore (unification runtime tp2 INT);
			ignore (unification runtime tp3 INT);
			(LIST INT) |
		NODEXPR -> 
			NOD |
		PAIREXPR (nd,exprs) ->
			let exprsType = List.map (evalExprType runtime) exprs in
			addDecoration runtime nd (PAIR exprsType);
			PAIR exprsType |
		LISTCOMPREHENSIONEXPR (nd,exprInList,pattern,exprValue) -> 
			let exprType = evalExprType runtime exprValue in
			let singleValueType = newGeneric() in
			ignore (unification runtime (LIST singleValueType) exprType);			
			let runtime1 = newScope runtime in
			let patternType = evalPatternType runtime1 pattern in
			ignore (unification runtime1 patternType singleValueType);
			let exprInListType = evalExprType runtime1 exprInList in
			addDecoration runtime nd (LIST exprInListType);
			LIST exprInListType | 
		ARRAYEXPR (nd,exprs) -> 
			let resultType = List.fold_left (fun resultType expr -> (unification runtime resultType (evalExprType runtime expr))) (newGeneric()) exprs in
			addDecoration runtime nd (ARRAY resultType);
			(ARRAY resultType) |
		OBJEXPR obj -> 
			evalObjectType runtime obj;
			OBJECT |
		TRANSITIONEXPR trans -> 
			evalTransitionExpr runtime trans;
			TRANSITION |
		TYPEVERIFICATIONEXPR (nd,expr,tpExpr) -> 
			let exprType = evalExprType runtime expr in
			let (_,typeType) = evalTypeType runtime [] tpExpr in
			let resultType = unification runtime typeType exprType in
			addDecoration runtime nd resultType;
			resultType |
		PROMISEEXPR (expr,newRuntime) -> 
			evalExprType newRuntime !expr 
and evalPatternType runtime pattern =
	match pattern with
		CONSPATTERN (nd,carPattern,cdrPattern) -> 
			let carType = evalPatternType runtime carPattern in
			let cdrType = evalPatternType runtime cdrPattern in
			let resultType = unification runtime (LIST carType) cdrType in
			addDecoration runtime nd resultType;
			resultType | 
		INTPATTERN _ -> 
			INT |
		FLOATPATTERN _ -> 
			FLOAT |
		STRINGPATTERN _ -> 
			STRING | 
		BOOLPATTERN _ -> 
			BOOL |
		LISTPATTERN (nd,lst) -> 
			let types = List.map (evalPatternType runtime) lst in 
			let singleType = List.fold_left (unification runtime) (newGeneric()) types in 
			addDecoration runtime nd (LIST singleType);
			(LIST singleType) |
		RENAMINGPATTERN (nd,pattern,id) -> 
			let patternType = evalPatternType runtime pattern in
			let idType = newGeneric() in 
			addId runtime id nd idType NODEXPR;
			let resultType = unification runtime idType patternType in
			addDecoration runtime nd resultType;
			resultType | 
		WILDCARDPATTERN nd -> 
			let resultType = newGeneric() in
			addDecoration runtime nd resultType;
			resultType |
		PAIRPATTERN (nd,patterns) -> 
			let patternTypes = List.map (evalPatternType runtime) patterns in
			addDecoration runtime nd (PAIR patternTypes);
			(PAIR patternTypes) | 
		IDPATTERN (nd,id) -> 
			(try
				(getIdValueCurrentLevel runtime id).cType
			with
				IdNotDeclared -> 
					let tp = newGeneric() in
					addId runtime id nd tp NODEXPR;
					tp) | 
		WHEREPATTERN (nd,pattern,expr) -> 
			let patternType = evalPatternType runtime pattern in
			let exprType = evalExprType runtime expr in
			ignore (unification runtime BOOL exprType);
			addDecoration runtime nd patternType;
			patternType | 
		ARRAYPATTERN (nd,lst) -> 
			let types = List.map (evalPatternType runtime) lst in 
			let singleType = List.fold_left (unification runtime) (newGeneric()) types in 
			addDecoration runtime nd (LIST singleType);
			(ARRAY singleType) |
		TYPEDPATTERN (nd,pattern,tp) -> 
			let patternType = evalPatternType runtime pattern in
			let (_,typeType) = evalTypeType runtime [] tp in
			let resultType = unification runtime patternType typeType in
			addDecoration runtime nd resultType;
			resultType
and evalTypeType runtime genericTypes tp =
	match tp with
		NODTYPE -> 
			(genericTypes,NOD) |
		INTTYPE -> 
			(genericTypes,INT) |
		FLOATTYPE -> 
			(genericTypes,FLOAT) |
		STRINGTYPE -> 
			(genericTypes,STRING) |
		ARRAYTYPE tp -> 
			let (_,evaluatedTp) = evalTypeType runtime [] tp in
			(genericTypes,(ARRAY evaluatedTp)) |
		LISTTYPE tp -> 
			let (_,evaluatedTp) = evalTypeType runtime [] tp in
			(genericTypes,(LIST evaluatedTp)) |
		GENTYPE id -> 
			(try
				(genericTypes,(ListTools.assoc (fun x -> (String.compare x id) == 0) genericTypes))
			with
				Not_found -> 
					let resultType = newGeneric() in
					(((id,resultType)::genericTypes),resultType)) |
		PAIRTYPE types -> 
			let (newGenericTypes,types) = ListTools.mapWithState (evalTypeType runtime) genericTypes types in
			(newGenericTypes,(PAIR types)) |
		NAMEDTYPE (id,types) ->
			let (newGenericTypes,types) = ListTools.mapWithState (evalTypeType runtime) genericTypes types in
			(genericTypes,(NAMED (id,types))) |
		VARIANTTYPE variants -> 
			let (newGenericTypes,variantTypes) = ListTools.mapWithState (fun currentState (id,typeDescription) -> let (nextState,tp) = (evalTypeType runtime currentState typeDescription) in (nextState,(id,tp))) genericTypes variants in 
			(newGenericTypes,(VARIANT variantTypes)) |  
		FUNCTIONTYPE (paramTypeExpr,resultTypeExpr) -> 
			let (genericTypes1,tp1) = evalTypeType runtime genericTypes paramTypeExpr in
			let (genericTypes2,tp2) = evalTypeType runtime genericTypes1 resultTypeExpr in
			(genericTypes2,(FUNCTION (tp1,tp2))) |
		OBJECTTYPE -> 
			(genericTypes,OBJECT) |
		TRANSITIONTYPE ->
			(genericTypes,TRANSITION)
and evalActionType runtime action =
	match action with
		ASSIGNACTION (id,expr) -> 
			ignore (evalExprType runtime expr); |
		ASSIGNRULEACTION (id,expr) -> 
			let tp = evalExprType runtime expr in
			let _ = unification runtime TRANSITION tp in
			ignore 0 |
		ASSIGNOBJECTACTION (id,expr) -> 
			let tp = evalExprType runtime expr in
			let _ = unification runtime OBJECT tp in
			ignore 0 |
		EXPRACTION expr -> 
			let tp = evalExprType runtime expr in
			let _ = unification runtime ACTION tp in
			ignore 0 |
		DOACTION expr -> 
			let tp = evalExprType runtime expr in
			let _ = unification runtime (LIST ACTION) tp in
			ignore 0 |
		DELETERULEACTION id -> 
			ignore 0 |
		DELETEOBJECTACTION id -> 
			ignore 0 |
		DEFINETYPEACTION (id,paramIds,typeExpr) -> 
			let generics = List.map (fun id -> (id,newGeneric())) paramIds in
			let realTypes = List.map (fun (_,tp) -> tp) generics in
			let (_,tp) = evalTypeType runtime generics typeExpr in
			addType runtime id tp realTypes |
		DEFINEACTION (nd,id,params,expr) -> 
			evalAssign runtime ((IDPATTERN (nd,id)),params,expr); |
		DEFINEEXTERNALACTION (nd,id,typeExpr) -> 
			let (_,tp1) = evalTypeType runtime [] typeExpr in
			let tp2 = evalPatternType runtime (IDPATTERN (nd,id)) in
			ignore (unification runtime tp1 tp2); |
		DEFINEOBJECTACTION (id,expr) -> 
			let tp = evalExprType runtime expr in
			let _ = unification runtime OBJECT tp in
			ignore 0 |
		DEFINERULEACTION (id,expr) -> 
			let tp = evalExprType runtime expr in 
			let _ = unification runtime TRANSITION tp in
			ignore 0 |
		DEFINEINTERFACE (id,driverId,ins,outs) -> 
			ignore 0
and evalObjectType runtime obj =
	match obj with
		SIMPLEOBJECT atts -> 
			List.iter (fun (_,expr) -> ignore (evalExprType runtime expr)) atts |
		OBJECT (interfaceId,atts) -> 
			List.iter (fun (_,expr,_) -> ignore (evalExprType runtime expr)) atts |
		TRANSIENTSIMPLEOBJECT atts -> 
			List.iter (fun (_,expr) -> ignore (evalExprType runtime expr)) atts |
		TRANSIENTOBJECT (interfaceId,atts) -> 
			List.iter (fun (_,expr,_) -> ignore (evalExprType runtime expr)) atts
and evalTransitionExpr runtime trans = 
	match trans with
		EXPRTRANS (objs,equivalentExpr) -> 
			let runtime1 = newScope runtime in
			List.iter (evalObjectPattern runtime1) objs;
			let tp = evalExprType runtime1 equivalentExpr in
			let _ = unification runtime1 OBJECT tp in
			ignore 0 |
		ACTIONTRANS (objs,actionExpr) -> 
			let runtime1 = newScope runtime in
			List.iter (evalObjectPattern runtime1) objs;
			let tp = evalExprType runtime1 actionExpr in
			let _ = unification runtime1 ACTION tp in
			ignore 0 
and evalObjectPattern runtime obj = 
	match obj with
		OBJPATTERN atts -> 
			List.iter (evalObjectAttributePattern runtime) atts 
and evalObjectAttributePattern runtime att = 
	match att with
		VALUEATTRIBUTEPATTERN (id,pattern) -> 
			let _ = evalPatternType runtime pattern in
			ignore 0 |
		PRESENTATTRIBUTEPATTERN id -> 
			ignore 0 |
		TYPEATTRIBUTEPATTERN (id,tp) -> 
			let _ = evalTypeType runtime [] tp in
			ignore 0
;;
