
class wildcardPatternObject = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    true

  method getIds() = 
    []

  method toTree() = 
    CartesianTree.WILDCARDPATTERN
    
end;;
