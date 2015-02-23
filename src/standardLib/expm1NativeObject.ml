
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class expm1Helper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (expm1 (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (expm1 (obj#returnFloat()))
end;;

class expm1NativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "expm1" (new expm1Helper)
end;;
