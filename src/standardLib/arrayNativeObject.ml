

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class arrayHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalArray obj = new BoolExpressionObject.boolExpressionObject true
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class arrayNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "array?" (new arrayHelper)
end;;
