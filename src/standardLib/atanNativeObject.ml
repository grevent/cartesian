
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class atanHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (ComplexTools._atan (obj#returnNum()))
end;;

class atanNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "atan" (new atanHelper)
end;;
