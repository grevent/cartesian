
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class coshHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (cosh (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (cosh (obj#returnFloat()))
end;;

class coshNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "cosh" (new coshHelper)
end;;
