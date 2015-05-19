
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class asinHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (asin (obj#returnNum()))
end;;

class asinNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "asin" (new asinHelper)
end;;
