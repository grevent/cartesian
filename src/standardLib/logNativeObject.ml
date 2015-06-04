
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class logHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (ComplexTools._ln (obj#returnNum()))
end;;

class logNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "log" (new logHelper)
end;;
