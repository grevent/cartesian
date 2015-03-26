
class contextActionObject contextExpr exprObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    let env = Env.newEnv parents in
    let context = contextExpr#eval env in
    
    if context#isObject() then
      ((exprObj#eval env)#returnAction())#exec ((context#returnObject())::parents);

  method preExec env idList =
    let (ids1,context1) = contextExpr#preEval env idList in
    let (ids2,expr2) = exprObj#preEval env idList in
    
    (idList,((new contextActionObject context1 expr2)
	     :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))

  method toString() = 
    "context "^(contextExpr#toString())^" "^(exprObj#toString())

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<contextActionObject>"^(contextExpr#toXml(n-1))^(exprObj#toXml(n-1))^"</contextActionObject>"
					  
end;;
