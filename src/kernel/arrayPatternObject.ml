
class arrayPatternObject lst =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in 
    
    if exprEval#isArray() then
      (BasicTools.array_fold_left2 (fun acc x y -> if (x#matchToExpression env y) then acc else false) true (Array.of_list lst) (exprEval#returnArray()))
    else
      false

  method toString() = 
    "[| "^(List.fold_left (fun acc pattern -> acc^(if (String.compare acc "" == 0) then "" else "; ")^(pattern#toString())) "" lst)^" |]"

	
end;;
