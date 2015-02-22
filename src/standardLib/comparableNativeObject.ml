

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class comparableHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
  
  method evalInt obj = new BoolExpressionObject.boolExpressionObject true
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject true
  method evalChar obj = new BoolExpressionObject.boolExpressionObject true
  method evalString obj = new BoolExpressionObject.boolExpressionObject true
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class comparableNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "comparable?" (new comparableHelper)
end;;
