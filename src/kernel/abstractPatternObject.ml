
class virtual ['expression] abstractPatternObject =
object 
  inherit RuntimeObject.runtimeObject

  method virtual matchToExpression: ('expression Env.env) -> 'expression -> bool
end;;
