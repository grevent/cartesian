
class actionExpressionObject (actions : AbstractExpressionObject.abstractExpressionObject list) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    Debug.debugStartMethod self "eval";
    let result = ((new actionExpressionObject (List.map (fun x -> 
      x#eval env) actions))
		  :> AbstractExpressionObject.abstractExpressionObject)
    in
    Debug.debugEndMethod self "eval" result;
    result
      
  method preEval env idList = 
    Debug.debugStartMethod self "preEval";
    let (newIdList,result) = (AbstractExpressionObject.listPreEval env idList actions (fun x -> new actionExpressionObject x)) in
    Debug.debugEndMethod self "eval" result;
    (newIdList,(result :> AbstractExpressionObject.abstractExpressionObject))

  method isAction() = 
    true
      
  method returnAction() = 
    Debug.debugStartMethod self "action";
    let result = (new ActionsObject.actionsObject 
       (List.map (fun x -> x#returnAction()) actions) )
    in
    Debug.debugEndMethod self "eval" result;
    result

  method toTree() = 
    CartesianTree.ACTIONEXPRESSION (List.map (fun x -> x#toTree()) actions)
      
end;;
