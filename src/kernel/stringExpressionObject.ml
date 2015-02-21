
class stringExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isString() = true
    
  method returnString() = vl
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method toString() = 
    "\""^vl^"\""
      

end;;
