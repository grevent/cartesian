
class intPatternObject it =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in

    if exprEval#isInt() then
      if exprEval#returnInt() == it then
	true
      else
	false
    else
      false

  method getIds() = 
    []

  method toString() = 
    (Printf.sprintf "%d" it)
	
end;;
