

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class expm1Helper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (ComplexTools._expm1 (obj#returnNum()))
end;;

class expm1NativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "expm1" (new expm1Helper)
end;;
