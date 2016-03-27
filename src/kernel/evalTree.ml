
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
	
let rec getMutablesExpr env expr = 
	match expr with 
		INTEXPR _ -> [] |
		FLOATEXPR _ -> [] |
		STRINGEXPR _ -> [] | 
		FUNCTIONCALLEXPR (_,fnExpr,paramExpr) -> (fusion compare (getMutablesExpr env fnExpr) (getMutablesExpr env paramExpr)) |
		BOOLEXPR _ -> [] |
		IDEXPR (_,id) -> 
			let (refNd,isMutable) = getIdDef env id in
			if isMutable then
				[refNd]
			else
				[] |
		ACTIONEXPR (_,actions) -> List.fold_left (fun acc action -> (fusion compare acc (getMutablesAction env action))) [] actions |
		LISTEXPR (_,lst) -> List.fold_left (fun acc expr -> (fusion compare acc (getMutablesExpr env expr))) [] lst |
		NODEXPR _ -> [] |
		ERROREXPR _ -> [] |
		PAIREXPR (_,vls) -> List.fold_left (fun acc expr -> (fusion compare acc (getMutablesExpr env expr))) [] vls |
		ARRAYEXPR (_,ar) -> List.fold_left (fun acc expr -> (fusion compare acc (getMutablesExpr env expr))) [] ar |
		LAMBDAEXPR (_,_,expr) -> getMutablesExpr env expr |
		VARIANTEXPR (_,_,expr) -> getMutablesExpr env expr |
		TYPEVERIFICATIONEXPR (_,expr,_) -> getMutablesExpr env expr | 
		TYPEACCESSEXPR (_,expr,_) -> getMutablesExpr env expr |
		GENERALISETYPEEXPR (_,expr,_) -> getMutablesExpr env expr |
		NARROWTYPEEXPR (_,expr,_) -> getMutablesExpr env expr |
		LETEXPR (_,assigns,expr) -> 
			let assignList = List.fold_left (fun acc (_,expr) -> (fusion compare (getMutablesExpr env expr) acc)) [] assigns in
			(fusion compare assignList (getMutablesExpr env expr)) |
		FUNCTIONEXPR (_,exprs) -> List.fold_left (fun acc expr -> (fusion compare (getMutablesExpr env expr) acc)) [] exprs 
and getMutablesAction env action = 
	match action with
		ASSIGNACTION (_,expr) -> getMutablesExpr env expr |
		EXPRACTION expr -> getMutablesExpr env expr | 
		DEFINETYPEACTION _ -> [] |
		DEFINEACTION (_,_,expr) -> getMutablesExpr env expr |
		DEFINEEXTERNALACTION _ -> []
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
			IL_VALUE (IV_ACTIONS (List.flatten (List.map (evalAction env) actions))) |			
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
		VARIANTEXPR (nd,str,expr) -> 
			let tmp = newReg() in
			IL_LET (nd,(evalExpr env expr),(IL_VALUE (IV_TAGGED (str,IV_REF tmp))))
and evalAction env action = 
	match action with
		ASSIGNACTION (id,expr) ->
			let (nd,_) = getIdDef env id in
			let mutables = (getMutablesExpr env expr) in
			let adapters = List.fold_left (fun currentAdapters id -> (IAD_INJECT (id,currentAdapters))) (IAD_EXPR (evalExpr env expr)) mutables in
			[IA_SET (id,adapters)] |
		EXPRACTION expr -> 
			[IA_COMPUTED (evalExpr env expr)] |
		DEFINETYPEACTION (_,_,_) -> 
			[] |
		DEFINEACTION (nd,id,expr) -> 
			let (nd,_) = getIdDef env id in
			let mutables = (getMutablesExpr env expr) in 
			let adapters = List.fold_left (fun currentAdapters id -> (IAD_INJECT (id,currentAdapters))) (IAD_EXPR (evalExpr env expr)) mutables in
			[ IA_DEFINE id; IA_SET (id,adapters) ] |
		DEFINEEXTERNALACTION (nd,str,tp) ->
			[ IA_DEFINEEXTERNAL str; ]
and evalPattern env pattern value = 
	match pattern with
		INTPATTERN (nd,vl) -> 
			(fun x -> 
				(createInternalCall3 IF_IF
					(createInternalCall2 IF_COMPAREINT (IL_VALUE (IV_INT vl)) value)
					x
					(IL_VALUE IV_ERROR) ) ) | 
		FLOATPATTERN (nd,vl) -> 
			(fun x -> (createInternalCall3 IF_IF
				(createInternalCall2 IF_COMPAREFLOAT (IL_VALUE (IV_FLOAT vl)) value)
				x
				(IL_VALUE IV_ERROR) ) ) |
		BOOLPATTERN (nd,vl) -> 
			(fun x -> (createInternalCall3 IF_IF
				(createInternalCall2 IF_COMPAREBOOL (IL_VALUE (IV_BOOL vl)) value)
				x
				(IL_VALUE IV_ERROR) ) ) |
		WILDCARDPATTERN nd -> 
			(fun x -> x) |
		STRINGPATTERN (nd,vl) -> 
			(fun x -> (createInternalCall3 IF_IF
				(createInternalCall2 IF_COMPARESTRING (IL_VALUE (IV_STRING vl)) value)
				x
				(IL_VALUE IV_ERROR) ) ) |
		TYPEDPATTERN (nd,pattern,tp) -> 
			(evalPattern env pattern value) | 
		IDPATTERN (nd,id) ->
			(try 
				let (refNd,isMutable) = getIdDef env id in
				(fun x -> 
					(createInternalCall3 IF_IF
						(createInternalCall2 IF_COMPARE (IL_VALUE (IV_REF refNd)) value)
						x
						(IL_VALUE IV_ERROR) ) )
			with _ ->
				(fun x -> (IL_LET (nd,value,x))) ) |
		WHEREPATTERN (nd,pattern,expr) -> 
			let patternCode = evalPattern env pattern value in
			let exprCode = evalExpr env expr in
			(fun x -> (patternCode (createInternalCall3 IF_IF exprCode x (IL_VALUE IV_ERROR)))) |
		CONSPATTERN (nd,pattern1,pattern2) -> 
			let carPattern = evalPattern env pattern1 (createInternalCall1 IF_CAR value) in
			let cdrPattern = evalPattern env pattern2 (createInternalCall1 IF_CDR value) in
			(fun x -> (carPattern (cdrPattern x))) |
		LISTPATTERN (nd,patterns) -> 
			let startReg = newReg() in
			let tmp = 
				(fun (reg,code) pattern -> 
					let cdrReg = newReg() in
					let carReg = newReg() in
					let patternCode = (evalPattern env pattern (IL_VALUE (IV_REF carReg))) in
					let newCode = (fun x -> (IL_LET (carReg,(createInternalCall1 IF_CAR (IL_VALUE (IV_REF reg))),
											(IL_LET (cdrReg,(createInternalCall1 IF_CDR (IL_VALUE (IV_REF reg))),
											(createInternalCall1 IF_STOPONERROR (patternCode x)) ) ) ) ) )
					in
					let resultCode = (fun x -> (code (newCode x))) in
					(cdrReg,resultCode) )
			in
			let (finalReg,newCode) = (List.fold_left 
				tmp
				(startReg,(fun x -> (IL_LET (startReg,value,x))))
				patterns )
			in 
			(fun x -> (newCode (createInternalCall3 IF_IF (createInternalCall1 IF_ISEMPTYLIST (IL_VALUE (IV_REF finalReg))) (IL_VALUE IV_ERROR) (createInternalCall1 IF_CAR (IL_VALUE (IV_REF finalReg)))))) |
		RENAMINGPATTERN (nd,pattern,id) -> 
			let codePattern = evalPattern env pattern value in
			(fun x -> (IL_LET (nd,(codePattern (IL_VALUE (IV_REF nd))),x))) |
		VARIANTPATTERN (nd,str,pattern) -> 
			let valueReg = newReg() in 
			let patternCode = evalPattern env pattern (createInternalCall1 IF_GETVARIANTVAL (IL_VALUE (IV_REF valueReg))) in
			(fun x -> 
				(IL_LET (valueReg,value,
				(createInternalCall3 IF_IF 
					(createInternalCall2 IF_COMPARESTRING (IL_VALUE (IV_STRING str)) (createInternalCall1 IF_GETVARIANTTAG (IL_VALUE (IV_REF valueReg))))
					(patternCode x) 
					(IL_VALUE IV_ERROR) ) ) ) ) |
		PAIRPATTERN (nd,patterns) ->
			let valueReg = newReg() in
			(ListTools.fold_left_i 
				(fun previousCode i pattern -> 
					let patternCode = evalPattern env pattern (createInternalCall2 IF_GETPAIRELEMENT (IL_VALUE (IV_REF valueReg)) (IL_VALUE (IV_INT i))) in
					(fun y -> previousCode (patternCode y)) )
				(fun y -> (IL_LET (valueReg,value,y)))
				patterns ) |
		ARRAYPATTERN (nd,patterns) -> 
			let valueReg = newReg() in
			(ListTools.fold_left_i 
				(fun previousCode i pattern -> 
					let patternCode = evalPattern env pattern (createInternalCall2 IF_GETARRAYELEMENT (IL_VALUE (IV_REF valueReg)) (IL_VALUE (IV_INT i))) in
					(fun y -> previousCode (patternCode y)) )
				(fun y -> (IL_LET (valueReg,value,y)))
				patterns )
;;

