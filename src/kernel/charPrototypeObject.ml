
class charPrototypeObject uc c = 
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new CharExpressionObject.charExpressionObject c
    
  method verifyValue vl = (vl#isChar() && (c == vl#returnChar()))

  method toString() = 
    "'"^(Char.escaped c)^"'"
    
end;;
      
