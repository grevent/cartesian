
class defineActionObject id patterns expr =
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents =
    Debug.debugStartMethod self "exec";
    (match parents with
       parent::_ -> 
       parent#addAttribute id 
			   (((if List.length patterns > 0 then
				(new FunctionExpressionObject.functionExpressionObject [(patterns,expr)])
	     else
	       expr))#eval (Env.newEnv parents))
     | _ -> raise AbstractActionObject.NoParents;);
    Debug.debugEndMethod self "exec" self;

  method preExec env idList =
    Debug.debugStartMethod self "preExec";
    let (newIds,newExpr) = (expr#preEval env (List.fold_left (fun acc pattern -> pattern#getIds()) (id::idList) patterns)) in
    let result = 
      ((new defineActionObject 
	  id 
	  patterns
	  newExpr)
       :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject))
    in
    Debug.debugEndMethod self "preExec" result;
    (newIds,result)

  method toTree() =
    CartesianTree.DEFINEACTION (id,(List.map (fun x -> x#toTree()) patterns),(expr#toTree()))
      
end;;
