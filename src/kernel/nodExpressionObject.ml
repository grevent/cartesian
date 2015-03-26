

class nodExpressionObject = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isNOD() = true
    
  method eval env = 
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))

  method toString() = 
    "nod"

  method toXml x = 
    match x with
      0 -> "..."
    | x -> "<nodExpressionObject/>"

end;;
