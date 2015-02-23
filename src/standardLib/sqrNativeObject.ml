
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class sqrHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (sqrt (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (sqrt  (obj#returnFloat()))
end;;

class sqrNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "sqr" (new sqrHelper)
end;;
