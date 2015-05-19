
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class sqrHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (sqrt  (obj#returnNum()))
end;;

class sqrNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "sqr" (new sqrHelper)
end;;
