

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class comparableHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
  
  method evalNum obj = new BoolExpressionObject.boolExpressionObject true
  method evalString obj = new BoolExpressionObject.boolExpressionObject true
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class comparableNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "comparable?" (new comparableHelper)
end;;
