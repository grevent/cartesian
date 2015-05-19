
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class expHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (exp (obj#returnNum()))
end;;

class expNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "exp" (new expHelper)
end;;
