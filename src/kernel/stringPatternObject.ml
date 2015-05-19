
class stringPatternObject st =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in

    if exprEval#isString() then
      if (compare (exprEval#returnString()) st) == 0 then
	true
      else
	false
    else
      false

  method getIds() = 
    []

  method toTree() = 
    CartesianTree.STRINGPATTERN st
	
end;;
