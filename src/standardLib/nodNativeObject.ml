

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class nodHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNOD obj = new BoolExpressionObject.boolExpressionObject true
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class nodNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "nod?" (new nodHelper)
end;;
