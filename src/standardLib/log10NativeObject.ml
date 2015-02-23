
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class log10Helper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (log10 (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (log10 (obj#returnFloat()))
end;;

class log10NativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "log10" (new log10Helper)
end;;
