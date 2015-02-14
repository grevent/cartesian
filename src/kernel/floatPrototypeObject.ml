
class floatPrototypeObject uc f = 
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new FloatExpressionObject.floatExpressionObject f
    
  method verifyValue vl = (vl#isFloat() && (f == vl#returnFloat()))

  method toString() = 
    (Printf.sprintf "%f" f)
    
end;;
      
