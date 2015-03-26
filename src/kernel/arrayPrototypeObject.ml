
class arrayPrototypeObject uc ar =
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = 
    new ArrayWrapperExpressionObject.arrayWrapperExpressionObject 
      (Array.map (fun x -> x#returnValue()) ar)
      
  method verifyValue vl = 
    if vl#isArray() then
      (BasicTools.array_fold_left2 (fun acc x y -> if (x#verifyValue y) then acc else false) true ar (vl#returnArray()))
    else
      false
	
  method toString() = 
    "[| "^
      (Array.fold_left 
	 (fun acc expr -> acc^(if (String.compare acc "" == 0) then "" else "; ")^(expr#toString()))
	 "" 
	 ar )
  ^" |]"

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<arrayPrototypeObject>...</arrayPrototypeObject>"
    | n -> 
      "<arrayPrototypeObject><useCase>"^
	uc^
	"</useCase>"^
	(Array.fold_left (fun acc x -> acc^(x#toXml(n-2))) "" ar)^
	"</arrayPrototypeObject>"
      

end;;
