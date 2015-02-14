
class assignActionObject (id: string) (exprObj: AbstractExpressionObject.abstractExpressionObject) =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec objs =
    match objs with
      obj::_ -> obj#setAttribute id (exprObj#eval (Env.newEnv objs))
    | _ -> raise AbstractActionObject.NoParents

  method toString() = 
    id^" <- "^(exprObj#toString())
    
end;;
