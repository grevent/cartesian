

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toFloatHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalInt obj = (new FloatExpressionObject.floatExpressionObject (float_of_int (obj#returnInt())))
  method evalFloat obj = obj
  method evalString obj = (new FloatExpressionObject.floatExpressionObject (float_of_string (obj#returnString())))
end;;

class toFloatNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toFloat" (new toFloatHelper)
end;;
