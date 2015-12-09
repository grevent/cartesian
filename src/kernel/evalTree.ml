
open CartesianDataModel
open EvalType

let typedEvalExpr runtime expr = 
	match expr with
		INTEXPR i -> expr 
;;

let evalExpr runtime expr = 
	let runtime1 = evalExprType runtime expr in
	typedEvalExpr runtime expr
and evalAction runtime action =
	let runtime1 = evalActionType runtime action in
	typedEvalExpr runtime1 action
;;
