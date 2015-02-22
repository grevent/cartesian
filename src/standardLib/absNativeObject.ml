
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class absHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject (abs (obj#returnInt()))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (abs_float (obj#returnFloat()))
end;;

class absNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "abs" (new absHelper)
end;;
