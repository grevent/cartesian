
class actionExpressionObject (actions : AbstractExpressionObject.abstractExpressionObject list) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    Debug.stdDebug (self#toXml 3) "eval" "<-" "";
    let result = ((new actionExpressionObject (List.map (fun x -> 
      x#eval env) actions))
		  :> AbstractExpressionObject.abstractExpressionObject)
    in
    Debug.stdDebug (self#toXml 3) "eval" "->" (result#toXml 3);
    result
      
  method preEval env idList = 
    Debug.stdDebug (self#toXml 3) "preEval" "<-" "";
    let (newIdList,result) = (AbstractExpressionObject.listPreEval env idList actions (fun x -> new actionExpressionObject x)) in
    Debug.stdDebug (self#toXml 3) "preEval" "->" (result#toXml 3);
    (newIdList,(result :> AbstractExpressionObject.abstractExpressionObject))

  method isAction() = 
    true
      
  method returnAction() = 
    Debug.stdDebug (self#toXml 3) "action" "<-" "";
    let result = (new ActionsObject.actionsObject 
       (List.map (fun x -> x#returnAction()) actions) )
    in
    Debug.stdDebug (self#toXml 3) "action" "->" (result#toXml 3);
    result
      
  method toString() = 
    "{ "^(List.fold_left (fun acc action -> acc^(action#toString())^"; ") "" actions)^" }"

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<actionExpressionObject>"^
	(List.fold_left (fun acc x -> acc^(x#toXml (n-1))) "" actions)
      ^"</actionExpressionObject>"

end;;
