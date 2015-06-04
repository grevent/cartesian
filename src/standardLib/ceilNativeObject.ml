
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class ceilHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    let (re,im) = obj#returnNum() in
    new NumExpressionObject.numExpressionObject ((ceil re),(ceil im))
end;;

class ceilNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "ceil" (new ceilHelper)
end;;
