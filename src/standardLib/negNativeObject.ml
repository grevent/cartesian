
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class negHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (~-. (obj#returnNum()))
end;;

class negNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "neg" (new negHelper)
end;;
