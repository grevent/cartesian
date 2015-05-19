
exception NotObject

class objectPatternObject attPatterns =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in

    if exprEval#isObject() then
      begin
	let objAttr = (exprEval#returnObject())#getAttributes() in
	List.fold_left 
	  (fun acc (attr,pattern) ->
	   try 
	     let (_,expr) =
	       (List.find (fun (realAttr,realExpr) -> if (String.compare realAttr attr) == 0 then true else false) objAttr)
	     in
	     (if acc then
		(pattern#matchToExpression env expr)
	      else
		false)
	   with _ -> false)
	  true attPatterns;
      end
    else
      raise NotObject
	
  method getIds() = 
    (List.fold_left (fun acc (att,vl) -> (vl#getIds())@acc) [] attPatterns)
      
  method toTree() =
    CartesianTree.OBJECTPATTERN (List.map (fun (attr,expr) -> (attr,expr#toTree())) attPatterns)

end;;
