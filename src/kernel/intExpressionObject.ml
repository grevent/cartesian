
class intExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject

  method isInt() = true
    
  method returnInt() = vl

  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))

  method toString() = 
    (Printf.sprintf "%d" vl)

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<intExpressionObject>"^(self#toString())^"</intExpressionObject>"

end;;
