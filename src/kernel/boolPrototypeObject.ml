
class boolPrototypeObject uc b =
object
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new BoolExpressionObject.boolExpressionObject b
    
  method verifyValue vl = (vl#isBool() && (b == vl#returnBool()))

  method toTree() = 
    CartesianTree.BOOLPROTOTYPE (uc,b)

end;;
      
