
open CartesianDataModel
open EvalType
open Tree

exception ArityNotOK
let rec applyValToFun runtime expr params = 
	match params with
		[] -> 
			expr |
		paramExpr::cdr -> 
			let lambdas = exprToLambda expr in
			let reducedLambdas = List.fold_left 
				(fun currentLambdas (params,expr) ->  
					match params with
						param::cdr -> 
							(try 
								evalPattern runtime param (PROMISE (ref paramExpr,copyRuntime runtime));
								currentLambdas@[(cdr,expr)]
							with 
								_ -> currentLambdas) |
						[] -> raise ArityNotOK)
				[] 
				lambdas 
			in
			applyValToFun runtime (LAMBDAEXPR ((-1),reducedLambdas)) cdr
and	evalExpr runtime expr = 
	match expr with
		INTEXPR i -> 
			INTEXPR i | 
		FLOATEXPR f -> 
			FLOATEXPR f |
		FUNCTIONCALLEXPR (nd,expr,exprs) -> 
			let evaluatedExpr = evalExpr runtime expr in
			let runtime1 = newScope runtime in
			applyValToFun runtime1 evaluatedExpr exprs;
			
;;

type exprNode = 
	FUNCTIONCALLEXPR of int*exprNode*(exprNode list) |
	LAMBDAEXPR of int*(((patternNode list)*exprNode) list) |
	LETEXPR of int*((patternNode*(patternNode list)*exprNode) list)*exprNode |	
	MATCHEXPR of int*exprNode*(((patternNode list)*exprNode) list) |
	MATCHPOSSIBLEEXPR of int*exprNode*(((patternNode list)*exprNode) list) |
	NARROWTYPEEXPR of int*exprNode*typeNode |
	GENERALISETYPEEXPR of int*exprNode*typeNode | 
	TYPEACCESSEXPR of int*exprNode*typeNode |
	TYPEVERIFICATIONEXPR of int*exprNode*typeNode |
	INTEXPR of int |
	FLOATEXPR of float |
	STRINGEXPR of string |
	BOOLEXPR of bool |
	IDEXPR of int*string | 
	ACTIONEXPR of int*(actionNode list) |
	LISTEXPR of int*(exprNode list) |
	INTERVALEXPR of exprNode*exprNode | 
	INTERVALSTEPEXPR of exprNode*exprNode*exprNode |
	NODEXPR |
	PAIREXPR of int*(exprNode list) |
	LISTCOMPREHENSIONEXPR of int*exprNode*patternNode*exprNode |
	ARRAYEXPR of int*(exprNode list) |
	OBJEXPR of objectNode |
	TRANSITIONEXPR of transitionNode
and patternNode = 
	CONSPATTERN of int*patternNode*patternNode |
	INTPATTERN of int |
	FLOATPATTERN of float |
	STRINGPATTERN of string |
	BOOLPATTERN of bool | 
	LISTPATTERN of int*(patternNode list) | 
	RENAMINGPATTERN of int*patternNode*string |
	WILDCARDPATTERN  of int |
	PAIRPATTERN of int*(patternNode list) | 
	IDPATTERN of int*string |
	WHEREPATTERN of int*patternNode*exprNode |
	ARRAYPATTERN of int*(patternNode list) |
	TYPEDPATTERN of int*patternNode*typeNode 
and typeNode = 
	NODTYPE |
	INTTYPE |
	FLOATTYPE |
	STRINGTYPE |
	ARRAYTYPE of typeNode |
	LISTTYPE of typeNode |
	GENTYPE of string |
	PAIRTYPE of (typeNode list) |
	NAMEDTYPE of string*(typeNode list) |
	VARIANTTYPE of ((string*typeNode) list) |
	FUNCTIONTYPE of (typeNode*typeNode) |
	OBJECTTYPE |
	TRANSITIONTYPE 
and actionNode =
	ASSIGNACTION of string*exprNode |
	ASSIGNRULEACTION of string*exprNode |
	ASSIGNOBJECTACTION of string*exprNode |
	DOACTION of exprNode |
	EXPRACTION of exprNode |
	DELETERULEACTION of string  |
	DELETEOBJECTACTION of string |
	DEFINETYPEACTION of string*(string list)*typeNode |
	DEFINEACTION of int*string*(patternNode list)*exprNode |
	DEFINEEXTERNALACTION of int*string*typeNode |
	DEFINEOBJECTACTION of string*exprNode |
	DEFINERULEACTION of string*exprNode |
	DEFINEINTERFACE of string*string*(string list)*(string list)
and objectNode =
	SIMPLEOBJECT of ((string*exprNode) list) | 
	OBJECT of (string*((string*exprNode*attributeInterfacing) list)) |
	TRANSIENTSIMPLEOBJECT of ((string*exprNode) list) |
	TRANSIENTOBJECT of (string*((string*exprNode*attributeInterfacing) list)) 
and attributeInterfacing = 
	SEND of string |
	RECEIVE of string |
	NOINTERFACE
and transitionNode =
	EXPRTRANS of (objectPatternNode list)*exprNode |
	ACTIONTRANS of ((objectPatternNode list)*exprNode) 
and objectPatternNode = 
	OBJPATTERN of (attributePatternNode list) 
and attributePatternNode = 
	VALUEATTRIBUTEPATTERN of string*patternNode |
	PRESENTATTRIBUTEPATTERN of string |
	TYPEATTRIBUTEPATTERN of string*typeNode
;;
