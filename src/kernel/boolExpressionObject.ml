
class boolExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isBool() = true
    
  method returnBool() = vl
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))

  method toString() = 
    if vl then "true" else "false"

  method toXml n = 
    match n with
      0-> "..."
    | n -> 
      "<boolExpressionObject>"^(self#toString())^"</boolExpressionObject>"

end;;
