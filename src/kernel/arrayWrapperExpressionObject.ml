
class arrayWrapperExpressionObject ar = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject 
    
  method eval env =
    (self :> AbstractExpressionObject.abstractExpressionObject)

  method preEval env idList = 
    AbstractExpressionObject.arrayPreEval env idList ar (fun x -> new arrayWrapperExpressionObject x)

  method isArray() = true
    
  method returnArray() = 
    ar

  method toString() = 
    "[| "^(Array.fold_left (fun acc expr -> acc^(if (String.compare acc "" == 0) then "" else "; ")^(expr#toString())) "" ar)^" |]"

  method toXml x = 
    match x with
      0 -> "..."
    | x -> "<arrayWrapperExpressionObject>"^
      (Array.fold_left (fun acc expr -> acc^(expr#toXml(x-1))) "" ar)
      ^"</arrayWrapperExpressionObject>"

end;;
