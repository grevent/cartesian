
class charPrototypeObject uc c = 
object(self)
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new CharExpressionObject.charExpressionObject c
    
  method verifyValue vl = (vl#isChar() && (c == vl#returnChar()))

  method toString() = 
    "'"^(Char.escaped c)^"'"
      
  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "charPrototypeObject>...</charPrototypeObject>"
    | n -> 
      "<charPrototypeObject><uc>"^uc^"</uc><val>"^(self#toString())^"</val></charPrototypeObject>"
    
end;;
      
