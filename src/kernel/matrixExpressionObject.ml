
class matrixExpressionObject mat =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject

  method isMatrix() = true
			
  method returnMatrix() =
    mat
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)
      
  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))
      
  method toTree() = 
    CartesianTree.MATRIXEXPRESSION mat

end;;
