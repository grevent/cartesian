
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class atanHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (atan (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (atan (obj#returnFloat()))
end;;

class atanNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "atan" (new atanHelper)
end;;
