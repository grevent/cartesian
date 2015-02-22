
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class sinHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (sin (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (sin (obj#returnFloat()))
end;;

class sinNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "sin" (new sinHelper)
end;;
