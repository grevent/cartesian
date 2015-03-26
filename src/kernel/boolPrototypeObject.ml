
class boolPrototypeObject uc b =
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new BoolExpressionObject.boolExpressionObject b
    
  method verifyValue vl = (vl#isBool() && (b == vl#returnBool()))

  method toString() = 
    if b then "true" else "false"

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<boolPrototypeObject>...</boolPrototypeObject>"
    | n -> 
      "<boolPrototypeObject><uc>"^uc^"</uc><val>"^(if b then "true" else "false")^"</val></boolPrototypeObject>"
    
end;;
      
