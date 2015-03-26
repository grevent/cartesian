
class listPatternObject lst =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in

    if exprEval#isList() then
      (List.fold_left2 (fun acc x y -> if (x#matchToExpression env y) then acc else false) true lst (exprEval#returnList()))
    else
      false

  method getIds() = 
    (List.fold_left (fun acc x -> (x#getIds())@acc) [] lst)

  method toString() = 
    (List.fold_left (fun acc el -> (if (String.compare "" acc) == 0 then "" else acc^" ")^(el#toString())) "" lst)

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<listPatternObject>"^(List.fold_left (fun acc x -> acc^(x#toXml(n-1))) "" lst)^"</listPatternObject>"
	
end;;
