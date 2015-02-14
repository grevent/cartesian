
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
      
  method toString() = 
    "let "^(List.fold_left (fun acc (pattern,expr) -> (if (String.compare "" acc) == 0 then "" else acc^" and ")^(pattern#toString())^" = "^(expr#toString())) "" assigns)^" in "^(exprObject#toString())
end;;
