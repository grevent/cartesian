
open Eval
open Tree

exception ActionNotImplemented of string;;

let rec exec env action =
	match action with 
		DOACTION expr -> 
			let newExpr = eval env expr in
			let actions = exprToActions newExpr in
			List.iter (exec env) actions |
		_ -> 
			Debug.genericDebug "Evaluation not possible, as not implemented...";
			raise (ActionNotImplemented (actionToString action));
;;
