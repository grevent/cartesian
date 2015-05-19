
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class sinHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (sin (obj#returnNum()))
end;;

class sinNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "sin" (new sinHelper)
end;;
