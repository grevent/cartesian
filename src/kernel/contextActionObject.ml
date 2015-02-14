
class contextActionObject contextExpr exprObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    let env = Env.newEnv parents in
    let context = contextExpr#eval env in
    
    if context#isObject() then
      ((exprObj#eval env)#returnAction())#exec ((context#returnObject())::parents);

  method toString() = 
    "context "^(contextExpr#toString())^" "^(exprObj#toString())
					  
end;;
