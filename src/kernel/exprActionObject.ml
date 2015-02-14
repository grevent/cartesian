
class exprActionObject exprObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    let env = Env.newEnv parents in
    let result = ((exprObj#eval env)#returnAction())#exec parents in
    result

  method toString() = 
    exprObj#toString()
					  
end;;
