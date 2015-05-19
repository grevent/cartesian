
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class ceilHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (ceil (obj#returnNum()))
end;;

class ceilNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "ceil" (new ceilHelper)
end;;
