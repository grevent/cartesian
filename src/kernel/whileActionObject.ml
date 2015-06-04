
class whileActionObject exprObj actionObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject

  method exec session parents = 
    while (exprObj#eval (Env.newEnv parents))#returnBoolean() do
      actionObj#exec session parents
    done;

  method preExec env idList = 
    let (_,newExpr) = exprObj#preEval env idList in
    let (_,newObj) = actionObj#preExec env idList in
    (idList,
     ((new whileActionObject newExpr newObj) :>
	 (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))

  method toTree() = 
    CartesianTree.WHILEACTION ((exprObj#toTree()),(actionObj#toTree()))
      
end;;
