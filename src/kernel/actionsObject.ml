
class actionsObject actions = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parent =
    List.iter (fun a -> a#exec parent) actions;

  method toString() = 
    List.fold_left (fun acc action -> acc^(action#toString())^"; ") "" actions
    
end;;
