
exception MoreThanOnePattern

open BasicTools

class tryActionObject objLst matchLst =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec session parents = 
    try 
      List.iter (fun x -> x#exec session parents) objLst;
    with
      RaiseActionObject.CartesianInternalException expr -> 
      let env = (Env.newEnv parents) in
	let expr = (new FunctionObject.functionObject matchLst)#apply env [expr] in
	let result = expr#eval env in
	(result#returnAction())#exec session parents;

  method preExec env idList = 
    let (_,newObjs) = statefullListMap 
      (fun state x -> (x#preExec env state) ) 
      idList 
      objLst
    in
    let (_,newMatchLst) = statefullListMap 
      (fun state (patterns,expr) -> 
	let ids = (List.fold_left (fun acc pattern -> acc@(pattern#getIds())) state patterns) in
	let (_,newExpr) = expr#preEval env ids in
	(state,(patterns,newExpr)) )
      idList
      matchLst
    in
    (idList,((new tryActionObject newObjs newMatchLst) :> 
	(AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))

  method toTree() =
    let objsTree = List.map (fun x -> (x#toTree())) objLst in
    let matchsTree = List.map (fun (patterns,expr) ->
			       let patternsTree = List.map (fun x -> (x#toTree())) patterns in
			       let exprTree = (expr#toTree()) in
			       (patternsTree,exprTree) ) matchLst in
    CartesianTree.TRYACTION (objsTree,matchsTree)

end;;
