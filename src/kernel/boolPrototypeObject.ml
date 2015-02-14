
class boolPrototypeObject uc b =
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new BoolExpressionObject.boolExpressionObject b
    
  method verifyValue vl = (vl#isBool() && (b == vl#returnBool()))

  method toString() = 
    if b then "true" else "false"
    
    
end;;
      
