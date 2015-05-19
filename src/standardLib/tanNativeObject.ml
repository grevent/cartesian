
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class tanHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (tan (obj#returnNum()))
end;;

class tanNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "tan" (new tanHelper)
end;;
