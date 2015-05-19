
class exprActionObject exprObj =
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents =
    Debug.debugStartMethod self "exec";
    let env = Env.newEnv parents in
    ((exprObj#eval env)#returnAction())#exec parents;
    Debug.debugEndMethod self "exec" self;

  method preExec env idList =
    Debug.debugStartMethod self "preExec";
    let (newIdList,newExprObj) = exprObj#preEval env idList in
    let result = ((new exprActionObject newExprObj)
		  :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)) in
    Debug.debugEndMethod self "preExec" result;
    (newIdList,result)

  method toTree() =
    CartesianTree.EXPRACTION (exprObj#toTree())
					  
end;;
