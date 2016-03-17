
open CartesianDataModel
open Type
open Env
open Tree
open ListTools

let regCounter = ref 0;;
let newReg() = 
	regCounter := !regCounter + 1;
	!regCounter
;;

let createInternalCall1 internal par1 = 
	(IL_CALL ((IL_VALUE (IV_INTERNAL internal)),par1))
;;

let createInternalCall2 internal par1 par2 = 
	(IL_CALL ((IL_CALL ((IL_VALUE (IV_INTERNAL internal)),par1)),par2))
;;

let createInternalCall3 internal par1 par2 par3 = 
	(IL_CALL ((IL_CALL ((IL_CALL ((IL_VALUE (IV_INTERNAL internal)),par1)),par2)),par3))
;;
	

let rec evalExpr env expr = 
	match expr with
		INTEXPR (_,i) -> 
			IL_VALUE (IV_INT i) |
		FLOATEXPR (_,f) -> 
			IL_VALUE (IV_FLOAT f) |
		STRINGEXPR (_,s) -> 
			IL_VALUE (IV_STRING s) |
		FUNCTIONCALLEXPR (nd,fnExpr,paramExpr) -> 
			let code1 = (evalExpr env fnExpr) in
			let code2 = (evalExpr env paramExpr) in 
			IL_CALL (code1,code2) |
		BOOLEXPR (_,b) -> 
			IL_VALUE (IV_BOOL b) |
		IDEXPR (_,id) -> 
			let (vl,_) = getIdDef env id in
			 IL_VALUE (IV_REF vl) |
		ACTIONEXPR (nd,actions) -> 
			IL_VALUE (IV_ACTIONS (evalAction env actions)) |			
		LISTEXPR (nd,listExpr) -> 
			let lets = List.map (fun expr -> (newReg(),(evalExpr env expr))) listExpr in
			List.fold_left (fun acc (nd,code) -> IL_LET (nd,code,acc)) (IL_VALUE (IV_LIST (List.map (fun (nd,_) -> IV_REF nd) lets))) (List.rev lets) |
		NODEXPR _ -> 
			IL_VALUE IV_NOD |
		ERROREXPR _ ->
			IL_VALUE IV_ERROR |
		PAIREXPR (nd,vls) -> 
			let lets = List.map (fun expr -> (newReg(),(evalExpr env expr))) vls in
			List.fold_left (fun acc (nd,code) -> IL_LET (nd,code,acc)) (IL_VALUE (IV_PAIR (List.map (fun (nd,_) -> IV_REF nd) lets))) (List.rev lets) |
		ARRAYEXPR (nd,listExpr) -> 
			let lets = List.map (fun expr -> (newReg(),(evalExpr env expr))) listExpr in
			List.fold_left (fun acc (nd,code) -> IL_LET (nd,code,acc)) (IL_VALUE (IV_ARRAY (Array.of_list (List.map (fun (nd,_) -> IV_REF nd) lets)))) (List.rev lets) |
		LAMBDAEXPR (nd,pattern,expr) -> 
			let tmp = newReg() in
			let patternFn = evalPattern env pattern (IL_VALUE (IV_REF tmp)) in
			IL_VALUE (IV_LAMBDA (tmp,patternFn (evalExpr env expr))) |
		NARROWTYPEEXPR (nd,expr,tp) -> 
			evalExpr env expr |
		NATIVEEXPR (_,_,fn) -> 
			IL_VALUE (IV_EXTERNAL fn) |
		GENERALISETYPEEXPR  (nd,expr,tp) -> 
			evalExpr env expr |
		LETEXPR (nd,assigns,expr) -> 
			List.fold_left 
				(fun resultCode (pattern,expr) -> let patternCode = evalPattern env pattern (evalExpr env expr) in
					(patternCode (evalExpr env expr)) )
				(evalExpr env expr)
				(List.rev assigns) |
		FUNCTIONEXPR (nd,lambdas) -> 
			let exprReg = newReg() in
			let lambdasCode = List.fold_left 
				(fun alternatives lambda -> 
					let lambdaCode = evalExpr env lambda in
					let lambdaCallCode = (IL_CALL (lambdaCode,(IL_VALUE (IV_REF exprReg)))) in
					(createInternalCall2 IF_ALTERNATIVEONERROR lambdaCallCode alternatives))
				(IL_VALUE IV_ERROR)
				(List.rev lambdas)
			in
			(IL_VALUE (IV_LAMBDA (exprReg,lambdasCode))) |
		TYPEACCESSEXPR (nd,expr,tp) -> 
			let pairType = getDecoration env (exprToId expr) in
			let pairTypes = getTypesFromPair pairType in
			let realTp = getDecoration env (typeToId tp) in
			let pos = firstWorkingPos (testUnification env realTp) pairTypes in
			(createInternalCall2 IF_PAIRACCESS (evalExpr env expr) (IL_VALUE (IV_INT pos))) |
		TYPEVERIFICATIONEXPR (nd,expr,tp) -> 
			evalExpr env expr |
		OBJEXPR (nd,obj) -> 
			IL_VALUE (IV_OBJECT (evalObj env obj)) |
		TRANSITIONEXPR (nd,transition) -> 
			evalTransition vars env transition |
		NATIVEEXPR (nd,tp,name) -> 
			getNative env name |
		VARIANTEXPR (nd,str,expr) -> 
			IL_VALUE (IV_VARIANT (str,(evalExpr vars env expr)))
and evalAction env action = 
	match action with
		ASSIGNACTION (id,expr) ->
			let (nd,_) = getIdDef env id in
			let mutables = (getMutables env expr) in
			let adapters = List.fold_left (fun currentAdapters id -> (IAD_INJECT (id,currentAdapters))) (IAD_EXPR (evalExpr env expr)) mutables in
			[IA_SET (nd,adapters)] |
		EXPRACTION expr -> 
			[IA_COMPUTED (evalExpr env expr)] |
		DEFINETYPEACTION (_,_,_) -> 
			[] |
		DEFINEACTION (nd,id,expr) -> 
			let (nd,_) = getIdDef env id in
			let mutables = (getMutables env expr) in 
			let adapters = List.fold_left (fun currentAdapters id -> (IAD_INJECT (id,currentAdapters))) (IAD_EXPR (evalExpr env expr)) mutables in
			[ DEFINE nd; IA_SET (nd,adapters) ]
and evalPattern env pattern value = 
	match pattern with
		INTPATTERN (nd,vl) -> 
			(fun x -> 
				(createInternalCall3 IF_IF
					(createInternalCall2 IF_COMPAREINT (IL_VALUE (IV_INT vl)) value)
					x
					(IL_VALUE IV_ERROR) ) ) | 
		FLOATPATTERN (nd,vl) -> 
			(fun x -> (IL_CALL (IL_CALL (IL_CALL (IL_VALUE (IV_INTERNAL IF_IF)),(IL_CALL (IL_CALL (IL_VALUE (IV_INTERNAL IF_COMPAREFLOAT),vl)),value)),x),(IL_VALUE IV_ERROR))) |
		BOOLPATTERN (nd,vl) -> 
			(fun x -> (IL_CALL (IL_CALL (IL_CALL (IL_VALUE (IV_INTERNAL IF_IF)),(IL_CALL (IL_CALL (IL_VALUE (IV_INTERNAL IF_COMPAREBOOL),vl)),value)),x),(IL_VALUE IV_ERROR))) |
		WILDCARDPATTERN nd -> 
			(fun x -> x) |
		STRINGPATTERN (nd,vl) -> 
			(fun x -> (IL_CALL (IL_CALL (IL_CALL (IL_VALUE (IV_INTERNAL IF_IF)),(IL_CALL (IL_CALL (IL_VALUE (IV_INTERNAL IF_COMPARESTRING),vl)),value)),x),(IL_VALUE IV_ERROR))) |
		TYPEDPATTERN (nd,pattern,tp) -> 
			(evalPattern env pattern value) | 
		IDPATTERN (nd,id) ->
			try 
				let (refNd,isMutable) = getIdDef env id in
				(fun x -> (IL_CALL (IL_CALL (IL_VALUE (IV_INTERNAL IF_IF)),(IL_CALL (IL_CALL (IL_VALUE (IV_INTERNAL IF_COMPARE)),(IL_VALUE (IV_REF refNd))),value),x)),(IL_VALUE IV_ERROR))
			with _ ->
				(fun x -> (IL_LET (nd,value,x))) |
		WHEREPATTERN (nd,pattern,expr) -> 
			let patternCode = evalPattern env pattern value in
			let exprCode = evalExpr env expr in
			(fun x -> (patternCode (createInternalCall3 IF_IF exprCode x (IL_VALUE IV_ERROR)))) |
		CONSPATTERN (nd,pattern1,pattern2) -> 
			let carPattern = evalPattern env pattern1 (createInternallCall1 IF_CAR value) in
			let cdrPattern = evalPattern env pattern2 (createInternalCall1 IF_CDR value) in
			(fun x -> (carPattern (cdrPattern x))) |
		LISTPATTERN (nd,patterns) -> 
			let startReg = newReg() in
			let (finalReg,newCode) = List.fold_left (fun (reg,code) pattern -> 
				let cdrReg = newReg() in
				let carReg = newReg() in
				let patternCode = evalPattern env IL_REG carReg in
				let newCode = (fun x -> (IL_LET (carReg,(IL_CALL (IL_VALUE (IV_INTERNAL IF_CAR)),(IL_REG reg)),
										(IL_LET (cdrReg,(IL_CALL (IL_VALUE (IV_INTERNAL IF_CDR)),(IL_REG reg)),
										(IL_CALL (IL_VALUE (IV_INTERNAL IF_STOPONERROR)),(patternCode x)) ) ) ) ) )
				in
				(cdrReg,(fun x -> (code (newCode x)))) ) 				
				(startReg,(fun x -> (IL_LET (startReg,value,x))))
				patterns
			in 
			(fun x -> (newCode (IL_CALL (IL_VALUE (IV_INTERNAL IF_CAR)),(IL_CALL (IL_VALUE (IV_INTERNAL IF_ISEMPTYLIST)),(IL_REF finalReg)),x,(IL_VALUE IV_ERROR)))) |
		RENAMINGPATTERN (nd,pattern,id) -> 
			let codePattern = evalPattern env pattern value in
			(fun x -> (IL_LET (nd,(codePattern (IL_REF nd)),x))) |
		VARIANTPATTERN (nd,str,pattern) -> 
			let valueReg = newReg() in 
			let patternCode = evalPatternCode env pattern (IL_CALL ((IL_VALUE (IV_INTERNAL IF_GETVARIANTVAL)),(IV_REF valueReg))) in
			(fun x -> 
				(IL_LET (valueReg,value,
				(createInternalCall3 IF_IF 
					(createInternalCall2 IV_COMPARESTRING (IL_VALUE (IV_STRING str)) (createInternalCall2 IV_GETVARIANTTAG (IV_REF valueReg)))
					(patternCode x) 
					(IL_VALUE IV_ERROR) ) ) ) ) |
		PAIRPATTERN (nd,patterns) ->
			let valueReg = newReg() in
			(ListTools.fold_left_i 
				(fun previousCode i pattern -> 
					let patternCode = evalPatternCode env pattern (createInternalCall2 IV_GETPAIRELEMENT (IV_REF valueReg) (IL_VALUE (IV_INT i))) in
					(fun y -> previousCode (patternCode y)) )
				(fun y -> (IL_LET (valueReg,value,y)))
				patterns ) |
		ARRAYPATTERN (nd,patterns) -> 
			let valueReg = newReg() in
			(ListTools.fold_left_i 
				(fun previousCode i pattern -> 
					let patternCode = evalPatternCode env pattern (createInternalCall2 IV_GETARRAYELEMENT (IV_REF valueReg) (IL_VALUE (IV_INT i))) in
					(fun y -> previousCode (patternCode y)) )
				(fun y -> (IL_LET (valueReg,value,y)))
				patterns )
and evalObj env obj = 
	match obj with 
		OBJECT lst -> 
			let lets = List.map (fun (str,expr) -> (str,newReg(),(evalExprCode env expr))) lst in
			let objExpr = List.map (fun (str,nd,_) -> (str,IL_REF nd)) lets in
			List.fold_left (fun acc (str,nd,code) -> (IL_LET (nd,code,acc))) (IL_VALUE (IV_OBJECT (List.map (fun (str,nd,_) -> (str,IV_REF nd)) lets))) (List.rev lets)
;;

