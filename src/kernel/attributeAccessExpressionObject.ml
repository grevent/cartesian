
class attributeAccessExpressionObject (exprObj: AbstractExpressionObject.abstractExpressionObject) id = 
object
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method preEval env idList = 
    ((new attributeAccessExpressionObject (exprObj#preEval env idList) id)
     :> AbstractExpressionObject.abstractExpressionObject)

  method eval env = 
    let expr = exprObj#eval env in
    let obj = expr#returnObject() in 
    let attr = obj#getAttribute id in

    if attr#isAction() then
      (new ActionExpressionObject.actionExpressionObject [(new ContextActionObject.contextActionObject expr attr)])
    else
      attr

  method toString() = 
    (exprObj#toString())^"."^id
  
end;;
