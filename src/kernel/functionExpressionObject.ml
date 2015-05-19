

class functionExpressionObject lambdaExprs =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    Debug.debugStartMethod self "eval";
    let preLambdas = (BasicTools.preEval lambdaExprs env []) in 
    let result = ((new functionExpressionObject preLambdas) :> AbstractExpressionObject.abstractExpressionObject)    in
    Debug.debugEndMethod self "eval" result;
    result
    
  method preEval env idList =
    Debug.debugStartMethod self "preEval";
    let preLambdas = (BasicTools.preEval lambdaExprs env idList) in 
    let result = ((new functionExpressionObject preLambdas) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.debugEndMethod self "preEval" result;
    (idList,result)
      
  method isFunction () = 
    true
    
  method returnFunction env =
    new FunctionObject.functionObject (BasicTools.preEval lambdaExprs env [])

  method toTree() =
    CartesianTree.FUNCTIONEXPRESSION (List.map (fun (patterns,expr) -> ((List.map (fun x -> x#toTree()) patterns),(expr#toTree()))) lambdaExprs)

end;;
