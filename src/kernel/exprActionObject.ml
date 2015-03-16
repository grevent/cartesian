
class exprActionObject exprObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    let env = Env.newEnv parents in
    let result = ((exprObj#eval env)#returnAction())#exec parents in
    result

  method preExec env idList = 
    let (newIdList,newExprObj) = exprObj#preEval env idList in
    (newIdList,((new exprActionObject newExprObj)
		:> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))

  method toString() = 
    exprObj#toString()
					  
end;;
