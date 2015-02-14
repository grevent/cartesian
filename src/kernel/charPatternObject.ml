
class charPatternObject ch =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
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

	
end;;
