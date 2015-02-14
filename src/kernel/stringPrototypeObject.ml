
class stringPrototypeObject uc s = 
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new StringExpressionObject.stringExpressionObject s
    
  method verifyValue vl = (vl#isString()) && (compare s (vl#returnString()) == 0)

  method toString() = 
    "\""^s^"\""
      
    
end;;
      
