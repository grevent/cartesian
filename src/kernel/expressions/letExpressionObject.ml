
exception LetMatchingException

class letExpressionObject (assigns: ((AbstractExpressionObject.abstractExpressionObject AbstractPatternObject.abstractPatternObject)*(AbstractExpressionObject.abstractExpressionObject AbstractPatternObject.abstractPatternObject list)*(AbstractExpressionObject.abstractExpressionObject)) list)  exprObject =
object
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    env#addLevel();
    
    List.iter (fun (pattern,params,expr) -> 
		let value = if List.length params > 0 then
				(new FunctionExpressionObject.functionExpressionObject [(params,expr)])#eval env
			else
				expr#eval env
		in
		if not (pattern#matchToExpression env value) then
			raise LetMatchingException;
		)
      assigns;
    let result = exprObject#eval env in
    env#removeLevel();
    result

  method preEval env idList = 
    let ids = List.fold_left (fun acc (pattern,params,expr) -> acc@(pattern#getIds())@(List.fold_left (fun acc param -> acc@(param#getIds())) [] params)) idList assigns in
    let pAssigns = List.map
      (fun (pattern,params,expr) -> 
	let (_,nextExpr) = expr#preEval env ids in
	(pattern,params,nextExpr) )
      assigns
    in
    let (_,finalExpr) = exprObject#preEval env ids in 
    (idList,((new letExpressionObject pAssigns finalExpr) :> AbstractExpressionObject.abstractExpressionObject))

  method toTree() =
    CartesianTree.LETEXPRESSION
      ((List.map (fun (pattern,params,expr) -> (pattern#toTree(),(List.map (fun x -> x#toTree()) params),expr#toTree())) assigns),(exprObject#toTree()))
      
end;;
