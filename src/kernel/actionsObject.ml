
class actionsObject (actions : AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject list) = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parent =
    List.iter 
      (fun a -> 
	Debug.actionExecDebug (Printf.sprintf "executing %s" (a#toString()));
	a#exec parent ) 
      actions;

  method preExec env idList =
    let (newIdList,newActions) = (List.fold_left 
				    (fun (newIdList,newActions) action -> 
				      let (nextIdList,nextAction) = action#preExec env newIdList in
				      (nextIdList,newActions@[nextAction]) )
				    (idList,[]) actions) 
    in
    (newIdList,((new actionsObject newActions) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject ))
      
  method toString() = 
    List.fold_left (fun acc action -> acc^(action#toString())^"; ") "" actions
    
end;;
