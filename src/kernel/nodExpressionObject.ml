

class nodExpressionObject = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isNOD() = true
    
  method eval env = 
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method toString() = 
    "nod"

end;;
