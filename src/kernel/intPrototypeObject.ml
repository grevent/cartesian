
class intPrototypeObject uc i = 
object(self)
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new IntExpressionObject.intExpressionObject i
    
  method verifyValue vl = (vl#isInt() && (i == vl#returnInt()))

  method toString() = 
    (Printf.sprintf "%d" i)

  method toXml x =
    match x with
      0 -> "..."
    | 1 -> "<intPrototypeObject>...</intPrototypeObject>"
    | _ -> "<intPrototypeObject><uc>"^uc^"</uc><val>"^(self#toString())^"</val></intPrototypeObject>"
    
end;;
      
