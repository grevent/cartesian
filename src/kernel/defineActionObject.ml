
class defineActionObject id patterns expr =
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    Debug.stdDebug (self#toXml 3) "exec" "<-" "";
    match parents with
      parent::_ -> 
	parent#addAttribute id 
	  (((if List.length patterns > 0 then
		(new FunctionExpressionObject.functionExpressionObject [(patterns,expr)])
	     else
	      expr))#eval (Env.newEnv parents))
    | _ -> raise AbstractActionObject.NoParents;
    Debug.stdDebug (self#toXml 3) "exec" "->" "";

  method preExec env idList = 
    Debug.stdDebug (self#toXml 3) "preExec" "<-" "";
    let (newIds,newExpr) = (expr#preEval env (List.fold_left (fun acc pattern -> pattern#getIds()) (id::idList) patterns)) in
    let result = 
      ((new defineActionObject 
	  id 
	  patterns
	  newExpr)
       :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject))
    in
    Debug.stdDebug (self#toXml 3) "preExec" "->" (result#toXml 3);
    (newIds,result)
      
  method toString() = "define "^id^(List.fold_left (fun acc pattern -> acc^(if (String.compare "" acc) == 0 then "" else " ")^(pattern#toString())) "" patterns)^" = "^(expr#toString())

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<defineActionObject>...</defineActionObject>"
    | n -> 
      "<defineActionObject><id>"^id^"</id>"^
	(List.fold_left (fun acc pattern -> acc^(pattern#toXml(n-1))) "" patterns)^
	(expr#toXml(n-1))^
	"</defineActionObject>"
      
end;;
