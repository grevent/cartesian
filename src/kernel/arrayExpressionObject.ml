
class arrayExpressionObject exprs = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env = 
    let resultArray = Array.init (List.length exprs) (fun i -> (List.nth exprs i)#eval env) in
    new ArrayWrapperExpressionObject.arrayWrapperExpressionObject resultArray

  method preEval env idList = 
    AbstractExpressionObject.listPreEval env idList exprs (fun x -> new arrayExpressionObject x)

  method toString() = 
    "[| "^(List.fold_left (fun acc expr -> acc^(if (String.compare acc "" == 0) then "" else "; ")^(expr#toString())) "" exprs)^" |]"      
   
  method toXml x = 
    match x with
      0 -> "..."
    | x -> "<arrayExpressionObject>"^
	(List.fold_left (fun acc expr -> acc^(expr#toXml(x-1))) "" exprs)^
	"<arrayExpressionObject/>"
 
end;;
