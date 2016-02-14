
open CartesianDataModel
open Type

let regCounter = ref 0;;
let newReg() = 
	regCounter := !regCounter + 1;
	!regCounter
;;

let rec evalExpr vars env expr = 
	match expr with
		INTEXPR i -> 
			IL_VALUE (IV_INT i) |
		FLOATEXPR f -> 
			IL_VALUE (IV_FLOAT f) |
		STRINGEXPR s -> 
			IL_VALUE (IV_STRING s) |
		FUNCTIONCALLEXPR (nd,fnExpr,paramExpr) -> 
			let code1 = (evalExpr vars env fnExpr) in
			let code2 = (evalExpr vars env paramExpr) in 
			IL_CALL (code1,code2) |
		BOOLEXPR b -> 
			IL_VALUE (IV_BOOL b) |
		IDEXPR (_,id) -> 
			let vl = List.assoc vars env id in
			 IL_VALUE (IV_REF vl) |
		ACTIONEXPR (nd,actions) -> 
			IL_VALUE (IV_ACTIONS (evalAction vars env actions)) |			
		LISTEXPR (nd,listExpr) -> 
			IL_VALUE (IV_LIST (List.map (evalExpr vars env) listExpr)) |
		NODEXPR -> 
			IL_VALUE IV_NOD |
		ERROREXPR ->
			IL_VALUE IV_ERROR |
		PAIREXPR (nd,vls) -> 
			IL_VALUE (IV_PAIR (List.map (evalExpr vars env) vls)) |
		ARRAYEXPR (nd,listExpr) -> 
			IL_VALUE (IV_ARRAY (Array.of_list (List.map (evalExpr vars env) listExpr))) |
		LAMBDAEXPR (nd,pattern,expr) -> 
			let tmp = newReg() in
			let (newVars,patternFn) = evalPattern vars pattern (IL_VALUE (REF i)) in
			IL_VALUE (IV_LAMBDA (tmp,patternFn (evalExpr newVars env expr))) |
		NARROWTYPEEXPR (nd,expr,tp) -> 
			evalExpr vars env expr |
		NATIVEEXPR (_,fn) -> 
			IL_VALUE (FUNCTION fn) |
		GENERALISETYPEEXPR  (nd,expr,tp) -> 
			evalExpr vars env expr |
		LETEXPR (nd,assigns,expr) -> 
			let newVars = List.fold_left (fun currentVars (pattern,expr) -> evalIdsInPattern currentVars pattern) vars assigns in			
			List.fold_left 
				(fun resultCode (pattern,expr) -> let (_,patternCode) = evalPattern newVars pattern (evalExpr newVars expr) in 
					(patternCode (evalExpr newVars env resultCode)) )
				(evalExpr newVars env expr)
				(List.rev assigns) |
		FUNCTIONEXPR (nd,lambdas) -> 
			let exprReg = newReg() in
			let lambdasCode = List.fold_left 
				(fun alternatives lambda -> 
					let lambdaCode = evalExpr vars env lambda in
					let lambdaCallCode = (IL_CALL (lambdaCode,(IL_VALUE (REF exprReg)))) in
					(IL_CALL (IL_CALL (IL_VALUE (INTERNAL ALTERNATIVEONERROR)),lambdaCallCode,alternatives)))
				(IL_VALUE ERROR)
				(List.rev lambdas)
			in
			(LAMBDA (exprReg,lambdasCode)) |
		TYPEACCESSEXPR (nd,expr,tp) -> 
			let pairType = getDecoration env (exprToId expr) in
			let pairTypes = getTypesFromPair pairType in
			let realTp = getDecoration env (typeToId tp) in
			let pos = firstWorkingPos (testUnification env realTp) pairTypes in
			(IL_CALL (IL_CALL (IL_VALUE (INTERNAL PAIRACCESS)),(evalExpr vars env expr)),pos) |
		TYPEVERIFICATIONEXPR (nd,expr,tp) -> 
			evalExpr vars env expr |
		OBJEXPR (nd,obj) -> 
			IL_VALUE (OBJECT (evalObj vars env obj)) |
		TRANSITIONEXPR (nd,transition) -> 
			evalTransition vars env transition |
		NATIVEEXPR (nd,tp,name) -> 
			getNative env name |
		VARIANTEXPR (nd,str,expr) -> 
			IL_VALUE (IV_VARIANT (str,(evalExpr vars env expr)))
;;


			
			


(* and evalPattern env pattern expr = (* Returns a Function that takes the resulting expression as parameter  *)
*)
