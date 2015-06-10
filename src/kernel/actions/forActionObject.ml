
class forActionObject id exprObj actionObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec session parents = 
    let expr = exprObj#eval (Env.newEnv parents) in
    
    if expr#isObject() then
      begin
	let lst = expr#returnObjectAsList() in 
	
	List.iter (fun x -> 
		   (new AssignActionObject.assignActionObject id x)#exec session parents;
		   actionObj#exec session parents;
		  ) lst;
      end
    else
      raise RuntimeObject.NotAListNorAVector

  method preExec env idList = 
    let (_,newExpr) = exprObj#preEval env (id::idList) in
    let (_,newAction) = actionObj#preExec env (id::idList) in
    
    (idList,((new forActionObject 
		id 
		newExpr
		newAction)
	     :> (AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))

  method toTree() =
    CartesianTree.FORACTION (id,(exprObj#toTree()),actionObj#toTree())

end;;
