
class intPrototypeObject uc i = 
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new IntExpressionObject.intExpressionObject i
    
  method verifyValue vl = (vl#isInt() && (i == vl#returnInt()))

  method toString() = 
    (Printf.sprintf "%d" i)

    
end;;
      
