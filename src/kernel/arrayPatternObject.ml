
class arrayPatternObject (lst: (AbstractExpressionObject.abstractExpressionObject AbstractPatternObject.abstractPatternObject) list)  =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 

  method getIds() = 
    (List.fold_left (fun acc pattern -> acc@(pattern#getIds())) [] lst)
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in 
    
    if exprEval#isArray() then
      (BasicTools.array_fold_left2 (fun acc x y -> if (x#matchToExpression env y) then acc else false) true (Array.of_list lst) (exprEval#returnArray()))
    else
      false

  method toString() = 
    "[| "^(List.fold_left (fun acc pattern -> acc^(if (String.compare acc "" == 0) then "" else "; ")^(pattern#toString())) "" lst)^" |]"

  method toXml x = 
    match x with
      0 -> "..."
    | x -> "<arrayPatternObject>"^
      (List.fold_left (fun acc pattern -> acc^(pattern#toXml(x-1))) "" lst)^
      "</arrayPatternObject>"
	
end;;
