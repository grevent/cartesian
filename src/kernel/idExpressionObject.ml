
class idExpressionObject (id: string) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    Debug.stdDebug (self#toXml 3) "eval" "<-" "";
    let result = env#get id in
    Debug.stdDebug (self#toXml 3) "eval" "->" (result#toXml 3);
    result

  method preEval env idList = 
    Debug.stdDebug (self#toXml 3) "preEval" "<-" "";
    Debug.genericDebug (env#toString());
    if (List.exists (fun x -> (String.compare x id) == 0) idList) then
      begin    
	Debug.stdDebug (self#toXml 3) "preEval" "->" (self#toXml 3);
	(idList,(self :> AbstractExpressionObject.abstractExpressionObject))
      end
    else 
      begin
	(try
	   let result = (env#get id) in
	   Debug.stdDebug (self#toXml 3) "preEval" "->" (result#toXml 3);
	   (idList,result)
	 with Env.IdNotDefined _ ->       
	   Debug.stdDebug (self#toXml 3) "preEval" "->" (self#toXml 3);
	   (idList,(self :> AbstractExpressionObject.abstractExpressionObject)))
      end

  method toString() = 
    id

  method toXml x = 
    match x with
      0 -> "..."
    | n -> "<idExpressionObject>"^id^"</idExpressionObject>"
      
end
