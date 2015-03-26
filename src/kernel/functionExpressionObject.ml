

class functionExpressionObject lambdaExprs =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    Debug.stdDebug (self#toXml(3)) "eval" "<-" "";
    let preLambdas = (BasicTools.preEval lambdaExprs env []) in 
    let result = ((new functionExpressionObject preLambdas) :> AbstractExpressionObject.abstractExpressionObject)    in
    Debug.stdDebug "functionExpressionObject" "eval" "->" (result#toXml(3));
    result
    
  method preEval env idList =
    Debug.stdDebug (self#toXml(3)) "preEval" "<-" "";
    let preLambdas = (BasicTools.preEval lambdaExprs env idList) in 
    let result = ((new functionExpressionObject preLambdas) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.stdDebug "functionExpressionObject" "preEval" "->" (result#toXml(3));
    (idList,result)
      
  method isFunction () = 
    true
    
  method returnFunction env =
    new FunctionObject.functionObject (BasicTools.preEval lambdaExprs env [])

  method toString() = 
    "lambda "^
      (List.fold_left (fun acc (patterns,expr) -> 
	acc^(if (String.compare acc "") == 0 then "" else "| ")^
	  (List.fold_left (fun acc pattern -> acc^(if (String.compare acc "") == 0 then "" else " ")^(pattern#toString())) "" patterns)
	^" "^(expr#toString())) "" lambdaExprs)

  method toXml x = 
    match x with
      0 -> "..."
    | x -> "<functionExpressionObject>"^
      (List.fold_left (fun acc (patterns,expr) -> 
	(List.fold_left (fun acc pattern -> acc^(pattern#toXml(x-1))) "" patterns)^(expr#toXml(x-1))) "" lambdaExprs)^
      "</functionExpressionObject>"

end;;
