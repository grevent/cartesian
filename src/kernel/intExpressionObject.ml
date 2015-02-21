
class intExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject

  method isInt() = true
    
  method returnInt() = vl

  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method toString() = 
    (Printf.sprintf "%d" vl)

end;;
