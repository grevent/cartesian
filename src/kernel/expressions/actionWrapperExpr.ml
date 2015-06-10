
class actionWrapperExpr realNativeAction = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    Debug.debugStartMethod self "eval";
    let (newIdList,newAction) = realNativeAction#preExec env [] in
    let result = ((new actionWrapperExpr newAction) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.debugEndMethod self "eval" result;
    result
      
  method preEval env idList = 
    Debug.debugStartMethod self "preEval";
    let (newIdList,newAction) = realNativeAction#preExec env idList in
    let result = ((new actionWrapperExpr newAction) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.debugEndMethod self "preEval" result;
    (newIdList,result)
      
  method isAction() = 
    true
      
  method returnAction() = 
    Debug.debugStartMethod self "returnAction";
    Debug.debugEndMethod self "returnAction" realNativeAction;
    realNativeAction

  method toTree() = 
    CartesianTree.ACTIONWRAPPER (realNativeAction#toTree())

end;;

