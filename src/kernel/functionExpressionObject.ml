
let preEval lambdas env idList = 
  (List.map (fun (patterns,expr) -> 
    let patternIds = List.fold_left (fun acc pattern -> acc@(pattern#getIds())) idList patterns in
    let (newIds,newExpr) = expr#preEval env patternIds in
    (patterns,newExpr) ) lambdas)
;;
    
class functionExpressionObject lambdaExprs =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    let preLambdas = (preEval lambdaExprs env []) in 
    ((new functionExpressionObject preLambdas) :> AbstractExpressionObject.abstractExpressionObject)    
    
  method preEval env idList =
    let preLambdas = (preEval lambdaExprs env idList) in 
    (idList,
     ((new functionExpressionObject preLambdas) :> AbstractExpressionObject.abstractExpressionObject))

  method isFunction () = 
    true
    
  method returnFunction env =
    new FunctionObject.functionObject (preEval lambdaExprs env [])

  method toString() = 
    "lambda "^
      (List.fold_left (fun acc (patterns,expr) -> 
	acc^(if (String.compare acc "") == 0 then "" else "| ")^
	  (List.fold_left (fun acc pattern -> acc^(if (String.compare acc "") == 0 then "" else " ")^(pattern#toString())) "" patterns)
	^" "^(expr#toString())) "" lambdaExprs)


end;;
