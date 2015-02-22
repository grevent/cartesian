

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class charHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalChar obj = new BoolExpressionObject.boolExpressionObject true
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class charNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "char?" (new charHelper)
end;;
