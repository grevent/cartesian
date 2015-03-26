
class attributeAccessExpressionObject (exprObj: AbstractExpressionObject.abstractExpressionObject) id = 
object
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method preEval env idList = 
    let (idsNew,exprNew) = exprObj#preEval env idList in
    (idsNew,((new attributeAccessExpressionObject exprNew id)
	     :> AbstractExpressionObject.abstractExpressionObject))
      
  method eval env = 
    let expr = exprObj#eval env in
    let obj = expr#returnObject() in 
    let attr = obj#getAttribute id in

    if attr#isAction() then
      (new ActionExpressionObject.actionExpressionObject [(new ActionWrapperExpr.actionWrapperExpr (new ContextActionObject.contextActionObject expr attr))])
    else
      attr

  method toString() = 
    (exprObj#toString())^"."^id

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<attributeAccessExpressionObject>...</attributeAccessExpressionObject>"
    | n -> 
      "<attributeAccessExpressionObject>"^(exprObj#toXml(n-2))^"<id>"^id^"</id></attributeAccessExpressionObject>"
  
end;;
