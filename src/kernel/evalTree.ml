
open CartesianDataModel
open EvalType
open Tree
open Runtime

exception PatternNotMatching
exception NoLambdaMatching

exception NoTypeForExpr
let rec getExprType runtime expr = 
	match expr with
		VARIANTEXPR (nd,_,_) -> getDecoration runtime nd |
		INTEXPR _ -> INT |
		FLOATEXPR _ -> FLOAT |
		STRINGEXPR _ -> STRING |
		FUNCTIONCALLEXPR (nd,_,_) -> getDecoration runtime nd |
		BOOLEXPR _ -> BOOL |
		IDEXPR (nd,_) -> getDecoration runtime nd |
		ACTIONEXPR (nd,_) -> getDecoration runtime nd |
		LISTEXPR (nd,_) -> getDecoration runtime nd |
		NODEXPR -> NOD |
		PAIREXPR (nd,_) -> getDecoration runtime nd |
		ARRAYEXPR (nd,_) -> getDecoration runtime nd |
		LAMBDAEXPR (nd,_,_) -> getDecoration runtime nd |
		INSTANCIATEDLAMBDAEXPR (nd,_,_,_) -> getDecoration runtime nd |
		NATIVEEXPR (tp,fn) -> tp |
		NARROWTYPEEXPR (nd,expr,tp) -> getDecoration runtime nd |
		GENERALISETYPEEXPR (nd,expr,tp) -> getDecoration runtime nd |
		INTERVALEXPR (st,en) -> LIST INT |
		INTERVALSTEPEXPR (st,en,step) -> LIST INT |
		LETEXPR (nd,assigns,expr) -> getDecoration runtime nd |
		MATCHEXPR (nd,expr,lambdas) -> getDecoration runtime nd |
		MATCHINLISTEXPR (nd,expr,lambdas) -> getDecoration runtime nd |
		MATCHINARRAYEXPR (nd,expr,lambdas) -> getDecoration runtime nd |
		TYPEACCESSEXPR (nd,expr,tp) -> getDecoration runtime nd |
		TYPEVERIFICATIONEXPR (nd,expr,tp) -> getDecoration runtime nd |
		OBJEXPR obj -> OBJECT |
		TRANSITIONEXPR trans -> TRANSITION |
		PROMISEEXPR (exprRef,_) -> getExprType runtime !exprRef |
		TBDEXPR -> raise NoTypeForExpr |
		LISTCOMPREHENSIONEXPR _ -> LIST INT
;;

let rec preEvalPattern runtime pattern = 
	match pattern with 
		INTPATTERN i0 -> 
			ignore 0 |
		FLOATPATTERN f0 -> 
			ignore 0 |
		STRINGPATTERN s0 -> 
			ignore 0 |
		BOOLPATTERN b0 -> 
			ignore 0 |
		IDPATTERN (nd,id) -> 
			(try 
				let _ = getIdValueCurrentLevel runtime id in
				ignore 0
			with IdNotDeclared -> 
				addId runtime id nd (getDecoration runtime nd) TBDEXPR;) |
		CONSPATTERN (nd,car,cdr) -> 
			preEvalPattern runtime car;
			preEvalPattern runtime cdr |
		LISTPATTERN (nd,lstPattern) -> 
			List.iter (preEvalPattern runtime) lstPattern |
		RENAMINGPATTERN (nd,pattern,id) -> 
			preEvalPattern runtime (IDPATTERN (nd,id));
			preEvalPattern runtime pattern; |
		WILDCARDPATTERN nd -> 
			ignore 0 |
		PAIRPATTERN (nd,patterns) -> 
			List.iter (preEvalPattern runtime) patterns; |
		TYPEDPATTERN (nd,pattern,tp) -> 
			preEvalPattern runtime pattern |
		WHEREPATTERN (nd,pattern,testExpr) -> 
			preEvalPattern runtime pattern |
		ARRAYPATTERN (nd,lstPattern) -> 
			List.iter (preEvalPattern runtime) lstPattern |
		VARIANTPATTERN (nd,id,pattern) -> 
			preEvalPattern runtime pattern
;;

let rec createFunction patterns expr =
	match patterns with
		[] -> expr |
		pattern::otherPatterns -> LAMBDAEXPR ((-1),pattern,(createFunction otherPatterns expr))
;;

let rec evalExpr runtime expr = 
	match expr with
		INTEXPR i -> 
			INTEXPR i | 
		FLOATEXPR f -> 
			FLOATEXPR f |
		STRINGEXPR s -> 
			STRINGEXPR s |
		FUNCTIONCALLEXPR (nd,fnExpr,paramExpr) -> 
			let evaluatedExpr = evalExpr runtime fnExpr in
			let runtime1 = newScope runtime in
			let (param,expr) = exprToLambda evaluatedExpr in
			let tmp = ref paramExpr in
			evalPattern runtime1 param (PROMISEEXPR (tmp,(copyRuntime runtime)));
			evalExpr runtime expr |
		BOOLEXPR b -> 
			BOOLEXPR b |
		IDEXPR (_,id) -> 
			let vl = getIdValue runtime id in
			vl.value |
		ACTIONEXPR (nd,actions) -> 
			ACTIONEXPR (nd,actions) |
		LISTEXPR (nd,listExpr) -> 
			LISTEXPR (nd,(List.map (evalExpr runtime) listExpr)) | 
		NODEXPR -> 
			NODEXPR |
		PAIREXPR (nd,vls) -> 
			PAIREXPR (nd,(List.map (evalExpr runtime) vls)) |
		ARRAYEXPR (nd,listExpr) -> 
			ARRAYEXPR (nd,(List.map (evalExpr runtime) listExpr)) | 
		LAMBDAEXPR (nd,pattern,expr) -> 
			INSTANCIATEDLAMBDAEXPR (nd,(copyRuntime runtime),pattern,expr) |
		INSTANCIATEDLAMBDAEXPR x -> 
			INSTANCIATEDLAMBDAEXPR x |
		NATIVEEXPR (tp,fn) -> 
			NATIVEEXPR (tp,fn) |
		NARROWTYPEEXPR (nd,expr,tp) -> 
			evalExpr runtime expr |
		GENERALISETYPEEXPR (nd,expr,tp) -> 
			evalExpr runtime expr |
		INTERVALEXPR (st,en) -> 
			let st_i = exprToInt (evalExpr runtime st) in
			let st_en = exprToInt (evalExpr runtime en) in
			let lst = ListTools.createList st_i st_en (fun x -> x + 1) (fun x -> x < st_en) in
			let resultLst = List.map (fun x -> INTEXPR x) lst in
			LISTEXPR ((-1),resultLst); |
		INTERVALSTEPEXPR (st,en,step) -> 
			let st_i = exprToInt (evalExpr runtime st) in
			let st_en = exprToInt (evalExpr runtime en) in
			let st_step = exprToInt (evalExpr runtime step) in
			let lst = ListTools.createList st_i st_en (fun x -> x + st_step) (fun x -> x < st_en) in
			let resultLst = List.map (fun x -> INTEXPR x) lst in
			LISTEXPR ((-1),resultLst); |
		LETEXPR (nd,assigns,expr) -> 
			let runtime1 = newScope runtime in
			List.iter (fun (pattern,_) -> preEvalPattern runtime1 pattern) assigns;
			List.iter (fun (pattern,expr) -> let tmp = ref expr in evalPattern runtime1 pattern (PROMISEEXPR (tmp,(copyRuntime runtime1)))) assigns;
			evalExpr runtime1 expr |
		MATCHEXPR (nd,expr,lambdas) -> 
			let _ = evalExpr runtime expr in
			(try
				let (_,firstPossible) = ListTools.firstWorking (evalExpr runtime) lambdas in
				firstPossible 
			with 
				Not_found -> 
					raise NoLambdaMatching) |
		MATCHINLISTEXPR (nd,expr,lambdas) -> 
			let valuesList = exprToList (evalExpr runtime expr) in
			let resultList = ListTools.allWorking 
				(fun element -> let tmp = evalExpr runtime (MATCHEXPR ((-1),element,lambdas)) in tmp)
				valuesList
			in
				LISTEXPR ((-1),resultList) |
		MATCHINARRAYEXPR (nd,expr,lambdas) -> 
			let valuesList = exprToArrayAsList (evalExpr runtime expr) in
			let resultList = ListTools.allWorking 
				(fun element -> let tmp = evalExpr runtime (MATCHEXPR ((-1),element,lambdas)) in tmp)
				valuesList
			in
				ARRAYEXPR (nd,resultList) |
		TYPEACCESSEXPR (nd,expr,tp) -> 
			let evaluatedExpr = evalExpr runtime expr in
			let pairValues = exprToPairsAsList evaluatedExpr in
			let evaluatedType = getDecoration runtime nd in
			let (result,_) = ListTools.firstWorking (fun el -> ignore (Type.unification runtime (getExprType runtime el) evaluatedType)) pairValues in
			result |
		TYPEVERIFICATIONEXPR (nd,expr,tp) -> 
			evalExpr runtime expr |
		LISTCOMPREHENSIONEXPR (nd,expr,pattern,listExpr) -> 
			let realList = evalExpr runtime listExpr in
			let result = List.map (fun x -> let runtime1 = newScope runtime in evalPattern runtime1 pattern x; evalExpr runtime1 expr) (exprToList realList) in
			LISTEXPR (nd,result) |
		OBJEXPR obj -> 
			OBJEXPR (evalObject runtime obj) |
		TRANSITIONEXPR trans -> 
			TRANSITIONEXPR (evalTransition runtime trans) |
		PROMISEEXPR (exprRef,runtime) -> 
			evalExpr runtime !exprRef |
		TBDEXPR -> 
			TBDEXPR |
		VARIANTEXPR (nd,id,expr) -> 
			VARIANTEXPR (nd,id,(evalExpr runtime expr))
and evalPattern runtime pattern expr = 
	match pattern with 
		INTPATTERN i0 -> 
			let i = exprToInt expr in 
			if i == i0 then
				ignore 0
			else
				raise PatternNotMatching |
		FLOATPATTERN f0 -> 
			let f = exprToFloat expr in
			if f == f0 then
				ignore 0
			else
				raise PatternNotMatching |
		STRINGPATTERN s0 -> 
			let s = exprToString expr in 
			if String.compare s s0 == 0 then
				ignore 0 
			else
				raise PatternNotMatching |
		BOOLPATTERN b0 -> 
			let b = exprToBool expr in 
			if b == b0 then
				ignore 0
			else
				raise PatternNotMatching |
		IDPATTERN (nd,id) -> 
			(try 
				let currentVal = getIdValueCurrentLevel runtime id in
				if exprEqual currentVal.value expr then
					ignore 0
				else
					raise PatternNotMatching
			with IdNotDeclared -> 
				addId runtime id nd (getDecoration runtime nd) expr;) |
		CONSPATTERN (nd,car,cdr) -> 
			let lst = exprToList expr in 
			evalPattern runtime car (List.hd lst);
			evalPattern runtime cdr (LISTEXPR (nd,(List.tl lst))) |
		LISTPATTERN (nd,lstPattern) -> 
			let lst = exprToList expr in 
			List.iter2 (evalPattern runtime) lstPattern lst |
		RENAMINGPATTERN (nd,pattern,id) -> 
			evalPattern runtime (IDPATTERN (nd,id)) expr;
			evalPattern runtime pattern expr; |
		WILDCARDPATTERN nd -> 
			ignore 0 |
		PAIRPATTERN (nd,patterns) -> 
			let lst = exprToPairsAsList expr in
			List.iter2 (evalPattern runtime) patterns lst; |
		TYPEDPATTERN (nd,pattern,tp) -> 
			evalPattern runtime pattern expr |
		WHEREPATTERN (nd,pattern,testExpr) -> 
			evalPattern runtime pattern expr;
			let testResult = evalExpr runtime testExpr in 
			let boolValue = exprToBool testResult in
			if boolValue then
				ignore 0 
			else
				raise PatternNotMatching |
		ARRAYPATTERN (nd,lstPattern) -> 
			let lst = exprToArrayAsList expr in 
			List.iter2 (evalPattern runtime) lstPattern lst |
		VARIANTPATTERN (nd,id,pattern) -> 
			let subExpr = variantToExpr id expr in 
			evalPattern runtime pattern subExpr 
and evalObject runtime obj = 
	match obj with
		SIMPLEOBJECT lst -> 
			SIMPLEOBJECT (List.map (fun (id,expr) -> (id,(evalExpr runtime expr))) lst) |
		OBJECT (driver,lst) ->  
			OBJECT (driver,List.map (fun (id,expr,interfaces) -> (id,(evalExpr runtime expr),interfaces)) lst) |
		TRANSIENTSIMPLEOBJECT lst -> 
			TRANSIENTSIMPLEOBJECT (List.map (fun (id,expr) -> (id,(evalExpr runtime expr))) lst) |
		TRANSIENTOBJECT (driver,lst) ->  
			TRANSIENTOBJECT (driver,List.map (fun (id,expr,interfaces) -> (id,(evalExpr runtime expr),interfaces)) lst)		
and evalTransition runtime obj = 
	match obj with
		EXPRTRANS (patterns,expr) -> 
			EXPRTRANS (patterns,PROMISEEXPR ((ref expr),(copyRuntime runtime))) |
		ACTIONTRANS (patterns,expr) -> 
			ACTIONTRANS (patterns,PROMISEEXPR ((ref expr),(copyRuntime runtime)))
;;
