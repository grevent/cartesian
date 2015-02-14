
class boolPatternObject bl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
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
	
end;;
