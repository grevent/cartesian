
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class log1pHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (log1p (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (log1p (obj#returnFloat()))
end;;

class log1pNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "log1p" (new log1pHelper)
end;;
