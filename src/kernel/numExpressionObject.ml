
class numExpressionObject (re,im) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject

  method isNum() = true
    
  method returnNum() =
    (re,im)
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))

  method toTree() = 
    CartesianTree.NUMEXPRESSION (re,im)

end;;
