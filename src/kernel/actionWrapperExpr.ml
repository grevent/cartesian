
class actionWrapperExpr realNativeAction = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    Debug.stdDebug (self#toXml 3) "eval" "<-" "";
    let (newIdList,newAction) = realNativeAction#preExec env [] in
    let result = ((new actionWrapperExpr newAction) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.stdDebug (self#toXml 3) "eval" "->" (result#toXml 3);
    result
      
  method preEval env idList = 
    Debug.stdDebug (self#toXml 3) "preEval" "<-" "";
    let (newIdList,newAction) = realNativeAction#preExec env idList in
    let result = ((new actionWrapperExpr newAction) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.stdDebug (self#toXml 3) "preEval" "->" (result#toXml 3);
    (newIdList,result)
      
  method isAction() = 
    true
      
  method returnAction() = 
    Debug.stdDebug (self#toXml 3) "returnAction" "<->" (realNativeAction#toXml 3);
    realNativeAction
      
  method toString () = 
    realNativeAction#toString()

  method toXml x = 
    match x with
      0 -> "..."
    | x -> "<actionWrapperExpr>"^(realNativeAction#toXml(x-1))^"<actionWrapperExpr/>"

end;;

