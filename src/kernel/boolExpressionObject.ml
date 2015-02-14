
class boolExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isBool() = true
    
  method returnBool() = vl
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method toString() = 
    if vl then "true" else "false"

end;;
