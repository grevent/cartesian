
class floatPrototypeObject uc f = 
object(self)
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new FloatExpressionObject.floatExpressionObject f
    
  method verifyValue vl = (vl#isFloat() && (f == vl#returnFloat()))

  method toString() = 
    (Printf.sprintf "%f" f)

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<floatPrototypeObject>...</floatPrototypeObject>"
    | n -> "<floatPrototypeObject><uc>"^uc^"</uc><val>"^(self#toString())^"</val></floatPrototypeObject>"
    
end;;
      
