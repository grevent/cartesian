
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class absHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (abs_float (obj#returnNum()))
end;;

class absNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "abs" (new absHelper)
end;;
