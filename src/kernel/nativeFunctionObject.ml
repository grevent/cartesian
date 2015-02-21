
class nativeFunctionObject implementation = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isFunction() = true
  method returnFunction env = implementation
  method eval env = (self :> AbstractExpressionObject.abstractExpressionObject)
  method preEval env idList = (self :> AbstractExpressionObject.abstractExpressionObject)

  method toString() = (implementation#toString())

end;;
