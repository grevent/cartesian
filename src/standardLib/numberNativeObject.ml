

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class numberHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject true
  method evalInt obj = new BoolExpressionObject.boolExpressionObject true
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class numberNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "number?" (new numberHelper)
end;;
