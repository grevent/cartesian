
exception CartesianInternalException of AbstractExpressionObject.abstractExpressionObject

class raiseActionObject exprObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    raise (CartesianInternalException (exprObj#eval (Env.newEnv parents)));

  method preExec env idList =
    let (_,newExpr) = exprObj#preEval env idList in
    
    (idList,((new raiseActionObject newExpr)
	     :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))

  method toTree() = 
    CartesianTree.RAISEACTION (exprObj#toTree())
      
end;;
