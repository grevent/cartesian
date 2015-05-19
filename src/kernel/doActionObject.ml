
class doActionObject actionObj exprObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject

  method exec parents =
    actionObj#exec parents;
    while (exprObj#eval (Env.newEnv parents))#returnBoolean() do
      actionObj#exec parents;
    done;

  method preExec env idList = 
    let (_,newAction) = actionObj#preExec env idList in
    let (_,newExpr) = exprObj#preEval env idList in
    
    (idList,((new doActionObject newAction newExpr)
	     :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))

  method toTree() =
    CartesianTree.DOACTION ((actionObj#toTree()),(exprObj#toTree()))
      
end;;
