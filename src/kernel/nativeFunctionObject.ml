
class nativeFunctionObject implementation = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isFunction() = true
  method returnFunction() = implementation
  method eval env = (self :> AbstractExpressionObject.abstractExpressionObject)
  method toString() = (implementation#toString())

end;;
