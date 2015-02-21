
class idExpressionObject (id: string) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    env#get id

  method preEval env idList = 
    if (List.exists (fun x -> (String.compare x id) == 0) idList) then
      (self :> AbstractExpressionObject.abstractExpressionObject)
    else 
      (try
	 env#get id
       with Env.IdNotDefined _ ->       
	 (self :> AbstractExpressionObject.abstractExpressionObject));

  method toString() = 
    id
end
