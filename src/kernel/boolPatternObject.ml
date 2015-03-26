
class boolPatternObject bl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 

  method getIds() = [] 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in
    
    if exprEval#isBool() then
      if exprEval#returnBool() == bl then
	true
      else
	false
    else
      false

  method toString() = 
    if bl then "true" else "false"

  method toXml x =
    match x with
      0 -> "..."
    | n -> 
      "<boolPatternObject>"^(if bl then "true" else "false")^"</boolPatternObject>"
	
end;;
