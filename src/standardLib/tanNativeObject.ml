
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class tanHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (tan (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (tan (obj#returnFloat()))
end;;

class tanNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "tan" (new tanHelper)
end;;
