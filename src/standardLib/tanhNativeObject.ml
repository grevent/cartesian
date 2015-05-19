
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class tanhHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (tanh (obj#returnNum()))
end;;

class tanhNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "tanh" (new tanhHelper)
end;;
