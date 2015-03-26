
class exprActionObject exprObj =
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    Debug.stdDebug (self#toXml 3) "exec" "<-" "";
    let env = Env.newEnv parents in
    ((exprObj#eval env)#returnAction())#exec parents;
    Debug.stdDebug (self#toXml 3) "exec" "->" "";

  method preExec env idList = 
    Debug.stdDebug (self#toXml 3) "preExec" "<-" "";
    let (newIdList,newExprObj) = exprObj#preEval env idList in
    let result = ((new exprActionObject newExprObj)
		  :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)) in
    Debug.stdDebug (self#toXml 3) "preExec" "->" (result#toXml 3);
    (newIdList,result)

  method toString() = 
    exprObj#toString()

  method toXml x = 
    match x with
      0 -> "..."
    | n -> "<exprActionObject>"^(exprObj#toXml(n-1))^"</exprActionObject>"
					  
end;;
