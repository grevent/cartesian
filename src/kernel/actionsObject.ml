
class actionsObject (actions : AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject list) = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parent =
    Debug.debugStartMethod self "exec";
    List.iter 
      (fun a -> 
	a#exec parent ) 
      actions;
    Debug.debugEndMethod self "exec" self;

  method preExec env idList =
    Debug.debugStartMethod self "preExec";
    let (newIdList,newActions) = (List.fold_left 
				    (fun (newIdList,newActions) action -> 
				      let (nextIdList,nextAction) = action#preExec env newIdList in
				      (nextIdList,newActions@[nextAction]) )
				    (idList,[]) actions) 
    in
    let result = ((new actionsObject newActions) :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject ) in
    Debug.debugEndMethod self "preExec" result;
    (newIdList,result)
      
  method toTree() = 
    CartesianTree.ACTIONS (List.map (fun x -> x#toTree()) actions)

end;;
