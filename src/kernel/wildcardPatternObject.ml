
class wildcardPatternObject = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    true

  method toString() = 
    "_"

  method getIds() = 
    []
	
end;;
