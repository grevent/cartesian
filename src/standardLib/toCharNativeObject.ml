

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toCharHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = (new CharExpressionObject.charExpressionObject (Char.chr (obj#returnInt())))
  method evalChar obj = obj
end;;

class toCharNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toChar" (new toCharHelper)
end;;
