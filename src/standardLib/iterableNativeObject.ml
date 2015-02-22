

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class iterableHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalArray obj = new BoolExpressionObject.boolExpressionObject true
  method evalString obj = new BoolExpressionObject.boolExpressionObject true
  method evalList obj = new BoolExpressionObject.boolExpressionObject true
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class iterableNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "iterable?" (new iterableHelper)
end;;
