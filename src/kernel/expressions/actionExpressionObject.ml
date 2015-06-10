
class actionExpressionObject (action: AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject) =
  
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    Debug.debugStartMethod self "eval";
    let (_,resultAction) = (action#preExec env []) in
    let result = ((new actionExpressionObject resultAction)
		  :> AbstractExpressionObject.abstractExpressionObject)
    in
    Debug.debugEndMethod self "eval" result;
    result
      
  method preEval env idList = 
    Debug.debugStartMethod self "preEval";
    let (newIdList,resultAction) = action#preExec env idList in
    let result = (new actionExpressionObject resultAction) in
    Debug.debugEndMethod self "eval" result;
    (newIdList,(result :> AbstractExpressionObject.abstractExpressionObject))

  method isAction() = 
    true
      
  method returnAction() = 
    Debug.debugStartMethod self "action";
    action
      
  method toTree() = 
    CartesianTree.ACTIONEXPRESSION (action#toTree())
      
end;;
