

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class boolHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalBool obj = new BoolExpressionObject.boolExpressionObject true
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class boolNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "bool?" (new boolHelper)
end;;
