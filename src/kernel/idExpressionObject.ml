
class idExpressionObject (id: string) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    Debug.debugStartMethod self "eval";
    let result = env#get id in
    Debug.debugEndMethod self "eval" result;
    result

  method preEval env idList = 
    Debug.debugStartMethod self "preEval";
    Debug.genericDebug (env#toString());
    if (List.exists (fun x -> (String.compare x id) == 0) idList) then
      begin    
	Debug.debugEndMethod self "preEval" self;
	(idList,(self :> AbstractExpressionObject.abstractExpressionObject))
      end
    else 
      begin
	(try
	   let result = (env#get id) in
	   Debug.debugEndMethod self "preEval" result;
	   (idList,result)
	 with Env.IdNotDefined _ ->       
	   Debug.debugEndMethod self "preEval" self;
	   (idList,(self :> AbstractExpressionObject.abstractExpressionObject)))
      end

  method toTree() = 
    CartesianTree.IDEXPRESSION id
      
end
