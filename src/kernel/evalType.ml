
open Tree 
open CartesianDataModel

exception PairElementsWithSameType;;
exception TypeNotInPair;;
let rec evalExprType runtime expr = 
	let evalAssign runtime (pattern,patterns,expr) = 
		if List.length patterns > 0 then
			begin
				let runtime1 = newScope runtime in
				let patternTypes = List.map (evalPatternType runtime1) patterns in
				let exprType = evalExprType runtime1 expr in
				let tp1 = FUNCTION (patternTypes,exprType) in
				let realTp1 = reduceGeneric runtime1 [] tp1 in
				let tp2 = evalPatternType runtime1 pattern in
				ignore (unification runtime1 realTp1 tp2);
			end
		else
			begin
				let exprType = evalExprType runtime expr in
				let tp =  evalPatternType runtime pattern in
				ignore (unification runtime1 exprType tp);
			end;
	in
	let evalMatch runtime (patterns,expr) =
		let runtime1 = newScope runtime in
		let patternTypes = List.map (evalPatternType runtime1) patterns in
		let exprType = evalExprType runtime1 expr in
		let tp = FUNCTION (patternTypes,exprType) in
		let realResult = reduceGeneric runtime1 [] tp in
		addDecoration nd realResult;
		realResult
	in
	match expr with
		FUNCTIONCALLEXPR (nd,fn,params) -> 
			let fnType = evalExprType runtime fn in
			let paramsType = List.map (evalExprType runtime) params in
			let resultType = newGeneric() in
			let tp = FUNCTION (paramsType,resultType) in
			unification runtime tp fnType;
			let realResult = reduceGeneric runtime [] resultType in
			addDecoration runtime nd realResult;
			realResult |
		LAMBDAEXPR (nd,patterns,expr) -> 
			let realResult = evalMatch runtime (patterns,expr) in
			addDecoration result nd realResult;
			realResult |
		LETEXPR (nd,assigns,expr) -> 
			let runtime1 = newScope runtime in
			List.iter (fun (pattern,patterns,expr) -> evalPatternType runtime1 pattern) assigns; 
			List.iter (evalAssign runtime1 x) assigns; 
			let realResult = evalExprType runtime1 expr in
			addDecoration runtime nd realResult;
			realResult |
		MATCHEXP (nd,expr,assigns) -> 
			let exprType = evalExprType runtime expr in
			let matchsType = List.map (evalMatch runtime)  assigns in
			let resultType = newGeneric() in
			let _ = List.fold (unification runtime) (FUNCTION (exprType,resultType)) matchsType in
			let realResult = reduceGeneric runtime [] resultType in
			addDecoration runtime nd realResult;
			realResult |
		MATCHPOSSIBLEEXPR (nd,expr,assigns) -> 
			let exprListType = evalExprType runtime expr in
			let exprType = newGeneric() in
			ignore (unification runtime (LIST exprType) exprListType);
			let matchsType = List.map (evalMatch runtime)  assigns in
			let resultType = newGeneric() in
			let _ = List.fold (unification runtime) (FUNCTION (exprType,resultType)) matchsType in
			let realResult = reduceGeneric runtime [] resultType in
			addDecoration runtime nd realResult;
			realResult |
		TYPEACCESSEXPR (nd,expr,typeNode) -> 
			let exprType = evalExprType runtime expr in
			let types = getTypesFromPairs exprType in
			if not (verifyUniqueness types) then
				raise PairElementsWithSameTypes;
			let typeType = evalTypeType runtime typeNode in
			if not (List.fold (fun result expr -> 
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
		NARROWTYPEEXPR (nd,expr,typeNode) ->
			let exprType = evalExprType runtime expr in
			let typeType = evalTypeType runtime typeNode in
			let resultType = subtypeUnification runtime typeNode exprType in
			addDecoration runtime nd resultType;
			resultType |
		GENERALISETYPEEXPR (nd,expr,typeNode) -> 
			let exprType = evalExprType runtime expr in 
			let typeType = evalTypeType runtime typeNode in
			let resultType = subtypeUnification runtime exprType typeNode in
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
			let resultType = List.fold_left (fun resultType expr -> (unification runtime (evalExprType runtime expr))) (newGeneric()) exprs in
			addDecoration runtime nd (LIST resultType);
			(LIST resultType) |
		INTERVALEXPR (expr1,expr2) ->
			let tp1 = evalExprType runtime expr1 in
			let tp2 = evalExprType runtime expr2 in
			ignore (unification runtime expr1 INT);
			ignore (unification runtime expr2 INT);
			(LIST INT) |
		INTERVALSTEPEXPR (expr1,expr2,expr3) -> 
			let tp1 = evalExprType runtime expr1 in
			let tp2 = evalExprType runtime expr2 in
			let tp3 = evalExprType runtime expr3 in
			ignore (unification runtime expr1);
			ignore (unification runtime expr2);
			ignore (unification runtime expr3);
			(LIST INT) |
		NODEXPR -> 
			NOD |
		PAIREXPR (nd,exprs) ->
			let exprsType = List.map (evalExprType runtime) exprs in
			addDecoration runtime (PAIR exprsType);
			PAIR exprsType |
		LISTCOMPREHENSIONEXPR (nd,exprInList,pattern,exprValue) -> 
			let exprType = evalExprType runtime exprValue in
			let singleValueType = newGeneric() in
			ignore (unification runtime (LIST singleEvalType) exprType);			
			let runtime1 = newScope runtime in
			let patternType = evalPatternType runtime1 pattern in
			ignore (unification runtime1 patternType singleValueType);
			let exprInListType = evalExprType runtime1 exprInList in
			addDecoration runtime (LIST exprInListType);
			LIST exprInListType | 
		ARRAYEXPR (nd,expr) -> 
			let resultType = List.fold_left (fun resultType expr -> (unification runtime (evalExprType runtime expr))) (newGeneric()) exprs in
			addDecoration runtime (ARRAY resultType);
			(ARRAY resultType) |
		OBJEXPR obj -> 
			evalObjType runtime obj;
			OBJECT |
		TRANSITIONEXPR trans -> 
			evalTransitionExpr runtime trans;
			TRANSITION
and evalPatternType runtime pattern =
	match pattern with
		CONSPATTERN (nd,carPattern,cdrPattern) -> 
			let carType = evalPatternNode runtime carPattern in
			let cdrType = evalPatternNode runtime cdrPattern in
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
			let singleType = List.fold_left (fun x -> (unification runtime (evalPatternType runtime x))) (newGeneric()) lst in 
			addDecoration runtime nd (LIST singleType);
			(LIST singleType) |
		RENAMINGPATTERN (nd,pattern,id) -> 
			let patternType = evalPatternNode runtime pattern in
			let idType = defineId runtime id in
			let resultType = unification runtime idType patternType in
			addDecoration runtime nd resultType;
			resultType | 
		WILDCARPATTERN nd -> 
			let resultType = newGeneric() in
			addDecoration runtime nd resultType;
			resultType |
		PAIRPATTERN (nd,patterns) -> 
			let patternTypes = List.map (evalPatternNode runtime) patterns in
			addDecoration runtime nd (PAIR patternsTypes);
			(PAIR patternTypes) |
		IDPATTERN (nd,id) -> 
			defineId runtime id | 
		WHEREPATTERN (nd,pattern,expr) -> 
			let patternType = evalPatternType runtime pattern in
			let exprType = evalExprType runtime expr in
			ignore (unification runtime BOOL exprType);
			addDecoration runtime nd patternType;
			patternType | 
		ARRAYPATTERN (nd,lst) -> 
			let singleType = List.fold_left (fun x -> (unification runtime (evalPatternType runtime x))) (newGeneric()) lst in
			addDecoration runtime nd (ARRAY singleType);
			(ARRAY singleType); |
		TYPEDPATTERN (nd,pattern,tp) -> 
			let patternType = evalPatternType runtime pattern in
			let typeType = evalTypeType runtime tp in
			let resultType = unification runtime patternType typeType in
			addDecoration runtime nd resultType;
			resultType 
and evalTypeType runtime tp =
	match tp with
		NODTYPE -> 
			NOD |
		INTTYPE -> 
			INT |
		FLOATTYPE -> 
			FLOAT |
		STRINGTYPE -> 
			STRING |
		ARRAYTYPE tp -> 
			ARRAY (evalTypeType runtime tp) |
		LISTTYPE tp -> 
			LIST (evalTypeType runtime tp) |
		GENTYPE st -> 
			try 
				ListTools.assoc (fun x y -> String.compare x y == 0) st !genericType
			with 
				Not_found -> 
					let tmpType = newGeneric() in
					genericType := (st,tmpType)::!genericType; |
		PAIRTYPE types -> 
			PAIR (List.map (evalTypeType genericType) types) |
		NAMEDTYPE id ->
			NAMED id |
		VARIANTTYPE variants -> 
			let variantsType = List.map (fun (id,typeDescription) -> (id,(evalTypeType runtime typeDescription))) variants in
			VARIANT variantsType |  
		FUNCTIONTYPE (paramTypeExpr,resultTypeExpr) -> 
			let tp1 = evalTypeType runtime paramTypeExpr in
			let tp2 = evalTypeType runtime resultTypeExpr in
			FUNCTION (tp1,tp2) |
		OBJECTTYPE -> 
			OBJECT |
		TRANSITIONTYPE ->
			TRANSITION 
and evalActionType runtime action =
	match action with
		ASSIGNACTION (id,expr) -> 
			





and actionNode =
	ASSIGNACTION of string*exprNode |
	DOACTION of exprNode |
	COPYACTION of string |
	EXPRACTION of exprNode |
	NEWACTION of string*exprNode |
	DELETEACTION of string  |
	REPLACEACTION of string*exprNode |
	DEFINETYPEACTION of string*(string list)*typeNode |
	DEFINEACTION of string*(patternNode list)*exprNode |
	EXTERNACTION of string*typeNode
and objectNode =
	OBJECT of syncMode*((string*exprNode) list)
and syncMode = 
	LOCAL | 
	INTERFACE of string
and transitionNode =
	EXPRTRANS of ((objectPatternNode list list)*exprNode) |
	ACTIONTRANS of ((objectPatternNode list list)*exprNode) 
and objectPatternNode = 
	OPENOBJPATTERN |
	OBJPATTERN of string*patternNode
;; 

