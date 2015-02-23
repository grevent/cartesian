
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class asinHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (asin (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (asin (obj#returnFloat()))
end;;

class asinNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "asin" (new asinHelper)
end;;
