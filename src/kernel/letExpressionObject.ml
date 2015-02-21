
exception LetMatchingException

class letExpressionObject assigns exprObject =
object
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    env#addLevel();
    
    List.iter (fun (pattern,expr) -> 
      let value = expr#eval env in
      if not (pattern#matchToExpression env value) then
	raise LetMatchingException;
    )
      assigns;
    let result = exprObject#eval env in
    env#removeLevel();
    result

  method preEval env idList = 
    let ids = List.fold_left (fun acc (pattern,expr) -> acc@(pattern#getIds())) idList assigns in
    let pAssigns = List.fold_left 
      (fun acc (pattern,expr) -> 
	((pattern,(expr#preEval env ids))::acc) )
      [] assigns
    in
    ((new letExpressionObject pAssigns (exprObject#preEval env ids))
     :> AbstractExpressionObject.abstractExpressionObject)
      
  method toString() = 
    "let "^(List.fold_left (fun acc (pattern,expr) -> (if (String.compare "" acc) == 0 then "" else acc^" and ")^(pattern#toString())^" = "^(expr#toString())) "" assigns)^" in "^(exprObject#toString())
end;;
