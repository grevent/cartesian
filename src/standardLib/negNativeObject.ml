
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class negHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject (~- (obj#returnInt()))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (~-. (obj#returnFloat()))
end;;

class negNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "neg" (new negHelper)
end;;
