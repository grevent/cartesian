
class defineObject id patterns expr = 
object(self)
  inherit FunctionCallObject.functionCallObject "define" ((new IdExpressionObject.idExpressionObject Id)::(new ListExpressionObject.listExpressionObject patterns)::expr::[])
end
