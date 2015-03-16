
class actionExpressionObject (actions : AbstractExpressionObject.abstractExpressionObject list) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    ((new actionExpressionObject (List.map (fun x -> x#eval env) actions))
     :> AbstractExpressionObject.abstractExpressionObject)
      
  method preEval env idList = 
    AbstractExpressionObject.listPreEval env idList actions (fun x -> new actionExpressionObject x)
      
  method isAction() = 
    true
      
  method returnAction() = 
    (new ActionsObject.actionsObject 
       (List.map (fun x -> x#returnAction()) actions) )

  method toString() = 
    "{ "^(List.fold_left (fun acc action -> acc^(action#toString())^"; ") "" actions)^" }"

end;;
