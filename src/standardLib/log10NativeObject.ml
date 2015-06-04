
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class log10Helper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (ComplexTools._log10 (obj#returnNum()))
end;;

class log10NativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "log10" (new log10Helper)
end;;
