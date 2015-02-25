
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class sinhHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (sinh (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (sinh (obj#returnFloat()))
end;;

class sinhNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "sinh" (new sinhHelper)
end;;
