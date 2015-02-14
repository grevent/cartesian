
class listWrapperExpressionObject lst = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env = 
    (self :> AbstractExpressionObject.abstractExpressionObject)
      
  method isList() = true
    
  method returnList() = lst

  method toString() = 
    "["^(List.fold_left (fun acc el -> acc^(el#toString())^"; ") "" lst)^"]"

end;;
