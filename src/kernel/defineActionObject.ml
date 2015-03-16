
class defineActionObject id patterns expr =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    match parents with
      parent::_ -> 
	parent#addAttribute id 
	  (((if List.length patterns > 0 then
	      (new FunctionExpressionObject.functionExpressionObject [(patterns,expr)])
	     else
	      expr))#eval (Env.newEnv parents))
    | _ -> raise AbstractActionObject.NoParents

  method preExec env idList = 
    let (newIds,newExpr) = (expr#preEval env (List.fold_left (fun acc pattern -> pattern#getIds()) (id::idList) patterns)) in
    (newIds,
     ((new defineActionObject 
	 id 
	 patterns
	 newExpr)
      :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))
      
  method toString() = "define "^id^(List.fold_left (fun acc pattern -> acc^(if (String.compare "" acc) == 0 then "" else " ")^(pattern#toString())) "" patterns)^" = "^(expr#toString())
      
end;;
