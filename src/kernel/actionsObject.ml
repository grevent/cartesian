
class actionsObject (actions : AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject list) = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parent =
    Debug.stdDebug (self#toXml(3)) "exec" "<-" "";
    List.iter 
      (fun a -> 
	a#exec parent ) 
      actions;
    Debug.stdDebug (self#toXml(3)) "exec" "->" "";

  method preExec env idList =
    Debug.stdDebug (self#toXml(3)) "preExec" "<-" "";
    let (newIdList,newActions) = (List.fold_left 
				    (fun (newIdList,newActions) action -> 
				      let (nextIdList,nextAction) = action#preExec env newIdList in
				      (nextIdList,newActions@[nextAction]) )
				    (idList,[]) actions) 
    in
    let result = ((new actionsObject newActions) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject ) in
    Debug.stdDebug (self#toXml(3)) "preExec" "->" (result#toXml(3));
    (newIdList,result)
      
  method toString() = 
    List.fold_left (fun acc action -> acc^(action#toString())^"; ") "" actions

  method toXml x = 
    match x with
      0 -> "..."
    | n -> "<actionsObject>"^
      (List.fold_left (fun acc action -> acc^(action#toXml(n-1))) "" actions)^
      "</actionsObject>"
    
end;;
