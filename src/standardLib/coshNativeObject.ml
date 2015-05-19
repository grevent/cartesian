
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class coshHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (cosh (obj#returnNum()))
end;;

class coshNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "cosh" (new coshHelper)
end;;
