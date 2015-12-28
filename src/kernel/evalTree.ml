
open CartesianDataModel
open EvalType

let typedEvalExpr runtime expr = 
	match expr with
		INTEXPR i -> expr 
;;

let evalExpr runtime expr = 
	let resultType = evalExprType runtime expr in
	(resultType,typedEvalExpr runtime expr)
and evalAction runtime action =
	(evalActionType runtime action);
	typedEvalAction runtime action 
;;

