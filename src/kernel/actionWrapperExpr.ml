
class actionWrapperExpr realNativeAction = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    (self :> AbstractExpressionObject.abstractExpressionObject)
      
  method preEval env idList = 
    let (newIdList,newAction) = realNativeAction#preExec env idList in
    (newIdList,((new actionWrapperExpr newAction) :> AbstractExpressionObject.abstractExpressionObject))
      
  method isAction() = 
    true
      
  method returnAction() = 
    realNativeAction
      
  method toString () = 
    realNativeAction#toString()

end;;

