
class quotedIdExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isId() = true
    
  method returnId() = vl
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList =
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))

  method toString() = 
    "'"^vl

end;;
