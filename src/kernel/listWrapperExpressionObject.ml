
class listWrapperExpressionObject (lst: AbstractExpressionObject.abstractExpressionObject list) = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env = 
    ((new listWrapperExpressionObject (List.map (fun x -> x#eval env) lst))
     :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    AbstractExpressionObject.listPreEval env idList lst (fun x -> new listWrapperExpressionObject x)
      
  method isList() = true
    
  method returnList() = 
    lst

  method toString() = 
    "["^(List.fold_left (fun acc el -> acc^(el#toString())^"; ") "" lst)^"]"

  method toXml x = 
    match x with
      0 -> "..."
    | x -> 
      "<listWrapperExpressionObject>"^
	(List.fold_left (fun acc el -> acc^(el#toXml(x-1))) "" lst)
      ^"</listWrapperExpressionObject>"

end;;
