
class listPrototypeObject uc lst =
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = 
    new ListWrapperExpressionObject.listWrapperExpressionObject (List.map (fun x -> x#returnValue()) lst)
      
  method verifyValue vl = 
    if vl#isList() then
      (List.fold_left2 (fun acc x y -> if (x#verifyValue y) then acc else false) true lst (vl#returnList()))
    else
      false

  method toString() = 
    (List.fold_left (fun acc el -> (if (String.compare acc "") == 0 then "" else acc^" ")^(el#toString())) "" lst)

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<listPrototypeObject>...</listPrototypeObject>"
    | n -> 
      "<listPrototypeObject><uc>"^uc^"</uc>"^
	(List.fold_left (fun acc x -> acc^(x#toXml(n-2))) "" lst)^
	"</charPrototypeObject>"

end;;
