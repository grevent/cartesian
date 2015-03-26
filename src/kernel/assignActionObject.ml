
class assignActionObject (id: string) (exprObj: AbstractExpressionObject.abstractExpressionObject) =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec objs =
    match objs with
      obj::_ -> obj#setAttribute id (exprObj#eval (Env.newEnv objs))
    | _ -> raise AbstractActionObject.NoParents

  method preExec env idList = 
    let (nextIdList,nextObj) = exprObj#preEval env idList in
    (nextIdList,
     ((new assignActionObject id nextObj)
      :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))

  method toString() = 
    id^" <- "^(exprObj#toString())

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<assignActionObject>...</assignActionObject>"
    | x -> "<assignActionObject><id>"^id^"</id>"^(exprObj#toXml(x-2))^"</assignActionObject>"
    
end;;
