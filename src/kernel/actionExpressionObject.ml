
class actionExpressionObject actions =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)
      
  method isAction() = true
    
  method returnAction() = 
    new ActionsObject.actionsObject actions

  method toString() = 
    "{ "^(List.fold_left (fun acc action -> acc^(action#toString())^"; ") "" actions)^" }"

end;;
