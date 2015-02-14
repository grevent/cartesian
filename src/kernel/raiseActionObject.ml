
exception CartesianInternalException of AbstractExpressionObject.abstractExpressionObject

class raiseActionObject exprObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    raise (CartesianInternalException (exprObj#eval (Env.newEnv parents)));

  method toString() = 
    "raise "^(exprObj#toString())
					  
end;;
