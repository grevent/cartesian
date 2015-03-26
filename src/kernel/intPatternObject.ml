
class intPatternObject it =
object(self)
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

  method toXml x =
    match x with
      0 -> "..."
    | n -> "<intPatternObject>"^(self#toString())^"</intPatternObject>"
	
end;;
