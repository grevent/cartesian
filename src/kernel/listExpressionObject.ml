
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

end;;
