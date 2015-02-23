
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class acosHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (acos (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (acos (obj#returnFloat()))
end;;

class acosNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "acos" (new acosHelper)
end;;
