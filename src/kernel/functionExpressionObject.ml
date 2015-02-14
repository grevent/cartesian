
class functionExpressionObject lambdaExprs =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method isFunction () = true
    
  method returnFunction() =
    new FunctionObject.functionObject lambdaExprs 

  method toString() = 
    "lambda "^
      (List.fold_left (fun acc (patterns,expr) -> 
	acc^(if (String.compare acc "") == 0 then "" else "| ")^
	  (List.fold_left (fun acc pattern -> acc^(if (String.compare acc "") == 0 then "" else " ")^(pattern#toString())) "" patterns)
	^(expr#toString())) "" lambdaExprs)


end;;
