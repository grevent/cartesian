
class arrayExpressionObject exprs = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    let resultArray = Array.init (List.length exprs) (fun i -> (List.nth exprs i)#eval env) in
    new ArrayWrapperExpressionObject.arrayWrapperExpressionObject resultArray

  method preEval env idList = 
    (new ArrayWrapperExpressionObject.arrayWrapperExpressionObject (Array.map (fun x -> x#preEval env idList) (Array.of_list exprs))
     :> AbstractExpressionObject.abstractExpressionObject)


  method toString() = 
    "[| "^(List.fold_left (fun acc expr -> acc^(if (String.compare acc "" == 0) then "" else "; ")^(expr#toString())) "" exprs)^" |]"      
    
end;;
