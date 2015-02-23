

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class lnotHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject ((lnot) (obj#returnInt()))
  method evalDefault obj = defaultValue
end;;

class lnotNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "lnot" (new lnotHelper)
end;;
