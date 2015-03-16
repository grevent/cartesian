
class idModificationActionObject (id: string) (expr: AbstractExpressionObject.abstractExpressionObject) (params: AbstractExpressionObject.abstractExpressionObject list) =
object
  inherit (ExprActionObject.exprActionObject 
	     (new FunctionCallExpressionObject.functionCallExpressionObject
		expr ((new IdExpressionObject.idExpressionObject id)::params)))
    
  method toString() = 
    id^" <- "^(expr#toString())^(List.fold_left (fun acc x -> " "^(x#toString())) "" params)
      
end;;
