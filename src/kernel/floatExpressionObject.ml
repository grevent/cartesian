
class floatExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject

  method isFloat() = true
    
  method returnFloat() = vl
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method toString() = 
    (Printf.sprintf "%f" vl)

end;;
