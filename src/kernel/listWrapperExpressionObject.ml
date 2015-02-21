
class listWrapperExpressionObject lst = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env = 
    ((new listWrapperExpressionObject (List.map (fun x -> x#eval env) lst))
     :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    ((new listWrapperExpressionObject (List.map (fun x -> x#preEval env idList) lst))
     :> AbstractExpressionObject.abstractExpressionObject)
      
  method isList() = true
    
  method returnList() = lst

  method toString() = 
    "["^(List.fold_left (fun acc el -> acc^(el#toString())^"; ") "" lst)^"]"

end;;
