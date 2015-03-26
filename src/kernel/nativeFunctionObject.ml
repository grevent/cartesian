
class nativeFunctionObject implementation = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isFunction() = true
  method returnFunction env = implementation
  method eval env = 
    Debug.stdDebug (self#toXml 3) "eval" "<->" (self#toXml 3);
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    Debug.stdDebug (self#toXml 3) "preEval" "<-" "";
    let (newIdList,newImplementation) = implementation#preEval env idList in
    let result = ((new nativeFunctionObject newImplementation) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.stdDebug (self#toXml 3) "preEval" "->" (result#toXml(3));
    (newIdList,result)
      
  method toString() = (implementation#toString())

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<nativeFunctionObject>"^(implementation#toXml(n-1))^"</nativeFunctionObject>"

end;;
