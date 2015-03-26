
class floatExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject

  method isFloat() = true
    
  method returnFloat() = vl
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))

  method toString() = 
    (Printf.sprintf "%f" vl)

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<floatExpressionObject>"^(self#toString())^"</floatExpressionObject>"

end;;
