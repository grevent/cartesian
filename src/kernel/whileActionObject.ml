
class whileActionObject exprObj actionObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject

  method exec parents = 
    while (exprObj#eval (Env.newEnv parents))#returnBoolean() do
      actionObj#exec parents
    done;

  method preExec env idList = 
    let (_,newExpr) = exprObj#preEval env idList in
    let (_,newObj) = actionObj#preExec env idList in
    (idList,
     ((new whileActionObject newExpr newObj) :>
	 (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))

  method toRepresentation() = 
    CartesianRepresentation.WHILEACTION ((exprObj#toRepresentation()),(actionObj#toRepresentation()))
      
end;;
