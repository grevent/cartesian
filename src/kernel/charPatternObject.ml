
class charPatternObject ch =
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 

  method getIds() = []
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in

    if exprEval#isChar() then
      if exprEval#returnChar() == ch then
	true
      else
	false
    else
      false

  method toString() = 
    "'"^(Char.escaped ch)^"'"

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<charPatternObject>"^(self#toString())^"</charPatternObject>"
	
end;;
