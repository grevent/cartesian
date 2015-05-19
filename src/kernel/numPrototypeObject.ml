
class floatPrototypeObject uc f = 
object(self)
  inherit AbstractPrototypeObject.abstractPrototypeObject uc
    
  method returnValue() = new FloatExpressionObject.floatExpressionObject f
    
  method verifyValue vl = (vl#isFloat() && (f == vl#returnFloat()))

  method toTree() =
    CartesianTree.FLOATPROTOTYPE (uc,f)
			    
end;;
      
