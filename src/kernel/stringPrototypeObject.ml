
class stringPrototypeObject uc s = 
object(self)
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new StringExpressionObject.stringExpressionObject s
    
  method verifyValue vl = (vl#isString()) && (compare s (vl#returnString()) == 0)

  method toString() = 
    "\""^s^"\""
      
  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<stringPrototypeObject>...</stringPrototypeObject>"
    | x -> 
      "<stringPrototypeObject><uc>"^uc^"</uc><val>"^(self#toString())^"</val></stringPrototypeObject>"
    
end;;
      
