

class nodExpressionObject = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isNOD() = true
    
  method eval env = 
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))

  method toTree() = 
    CartesianTree.NODEXPRESSION

end;;
