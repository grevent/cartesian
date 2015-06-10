
class numPatternObject fl =
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in

    if exprEval#isNum() then
      if exprEval#returnNum() == fl then
	true
      else
	false
    else
      false

  method getIds() =
    []

  method toTree() =
    CartesianTree.NUMPATTERN fl

end;;
