
class charExpressionObject vl =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method isChar() = true
    
  method returnChar() = vl
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList =
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject))

  method toString() = 
    "'"^(Char.escaped vl)^"'"

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<charExpressionObject>"^(self#toString())^"<charExpressionObject/>"

end;;
