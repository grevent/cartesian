

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class lnotHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    new NumExpressionObject.numExpressionObject (float_of_int ((lnot) (obj#returnNumAsInt())))
  method evalDefault obj = defaultValue
end;;

class lnotNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "lnot" (new lnotHelper)
end;;
