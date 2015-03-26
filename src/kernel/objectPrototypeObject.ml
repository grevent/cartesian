
class objectPrototypeObject uc lst =
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc 
    
  method returnValue() = 
    let obj = new ObjectObject.objectObject in
    
    List.iter (fun (attr,proto) -> obj#addAttribute attr (proto#returnValue())) lst;

    new ObjectWrapperExpressionObject.objectWrapperExpressionObject obj;

  method verifyValue vl = 
    let obj = vl#returnObject() in
    (List.fold_left (fun acc (attr,proto) -> if (proto#verifyValue (obj#getAttribute attr)) then acc else false) true lst)

  method toString() = 
    "{ "^(List.fold_left (fun acc (attribute,expr) -> 
      acc^attribute^"= "^(expr#toString())) "" lst)^" }"

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<objectPrototypeObject>...</objectPrototypeObject>"
    | x -> 
      "<objectPrototypeObject><uc>"^uc^"</uc>"^
	(List.fold_left (fun acc (id,expr) -> "<id>"^id^"</id>"^(expr#toXml(x-2))) "" lst)^
	"</objectPrototypeObject>"
      
end;;
