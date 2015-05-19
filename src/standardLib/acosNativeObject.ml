
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class acosHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (acos (obj#returnNum()))
end;;
  
class acosNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "acos" (new acosHelper)
end;;
