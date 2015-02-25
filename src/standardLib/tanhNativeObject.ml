
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class tanhHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (tanh (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (tanh (obj#returnFloat()))
end;;

class tanhNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "tanh" (new tanhHelper)
end;;
