
class boolExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isBool() = true
    
  method returnBool() = vl
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))

  method toTree() = 
    CartesianTree.BOOLEXPRESSION vl

end;;
