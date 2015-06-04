
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class log1pHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (ComplexTools._log1p (obj#returnNum()))
end;;

class log1pNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "log1p" (new log1pHelper)
end;;
