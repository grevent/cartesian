
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class logHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (log (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (log (obj#returnFloat()))
end;;

class logNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "log" (new logHelper)
end;;
