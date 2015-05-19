
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class sinhHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (sinh (obj#returnNum()))
end;;

class sinhNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "sinh" (new sinhHelper)
end;;
