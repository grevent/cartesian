
class stringPrototypeObject uc s = 
object(self)
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new StringExpressionObject.stringExpressionObject s
    
  method verifyValue vl = (vl#isString()) && (compare s (vl#returnString()) == 0)

  method toTree() = 
    CartesianTree.STRINGPROTOTYPE (uc,s)

end;;
      
