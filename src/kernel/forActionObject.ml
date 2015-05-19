
class forActionObject id exprObj actionObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    let expr = exprObj#eval (Env.newEnv parents) in
    
    if expr#isList() then
      begin
	let lst = expr#returnList() in 
	
	List.iter (fun x -> 
	  (new AssignActionObject.assignActionObject id x)#exec parents;
	  actionObj#exec parents;
	) lst;
	
      end
    else if expr#isArray() then
      begin
	let arr = expr#returnArray() in 
	
	Array.iter (fun x -> 
	  (new AssignActionObject.assignActionObject id x)#exec parents;
	  actionObj#exec parents;
	) arr;
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
