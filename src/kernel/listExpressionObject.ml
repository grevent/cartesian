
class listExpressionObject lst = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env = 
    let resultList = List.map (fun x -> x#eval env) lst in
    new ListWrapperExpressionObject.listWrapperExpressionObject resultList

  method preEval env idList = 
    AbstractExpressionObject.listPreEval env idList lst (fun x -> new listExpressionObject x)
      
  method toString() = 
    "["^(List.fold_left (fun acc el -> acc^(el#toString())^"; ") "" lst)^"]"

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<listExpressionObject>"^(List.fold_left (fun acc x -> acc^(x#toString())) "" lst)^"</listExpressionObject>"

end;;
