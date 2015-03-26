
class floatPatternObject fl =
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in

    if exprEval#isFloat() then
      if exprEval#returnFloat() == fl then
	true
      else
	false
    else
      false

  method getIds() =
    []

  method toString() = 
    (Printf.sprintf "%f" fl)

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<floatPatternObject>"^(self#toString())^"</floatPatternObject>"
	
end;;
