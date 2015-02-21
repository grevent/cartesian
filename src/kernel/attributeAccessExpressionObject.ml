
class attributeAccessExpressionObject (exprObj: AbstractExpressionObject.abstractExpressionObject) id = 
object
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method preEval env idList = 
    ((new attributeAccessExpressionObject (exprObj#preEval env idList) id)
     :> AbstractExpressionObject.abstractExpressionObject)

  method eval env = 
    let expr = exprObj#eval env in
    let obj = expr#returnObject() in 
    obj#getAttribute id

  method toString() = 
    (exprObj#toString())^"."^id
  
end;;
