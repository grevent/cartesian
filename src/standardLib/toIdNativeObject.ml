
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toIdHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = (new IdExpressionObject.idExpressionObject (obj#returnString()))
end;;

class toIdNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toId" (new toIdHelper)
end;;
