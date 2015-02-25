

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toStringHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalInt obj = (new StringExpressionObject.stringExpressionObject (obj#toString()))
  method evalFloat obj = (new StringExpressionObject.stringExpressionObject (obj#toString()))
  method evalList obj = (new StringExpressionObject.stringExpressionObject (obj#toString()))
  method evalBool obj = (new StringExpressionObject.stringExpressionObject (obj#toString()))
  method evalChar obj = (new StringExpressionObject.stringExpressionObject (obj#toString()))
  method evalObj obj = (new StringExpressionObject.stringExpressionObject (obj#toString()))
  method evalFunction obj = (new StringExpressionObject.stringExpressionObject (obj#toString()))
  method evalArray obj = (new StringExpressionObject.stringExpressionObject (obj#toString()))
  method evalAction obj = (new StringExpressionObject.stringExpressionObject (obj#toString()))
  method evalString obj = obj
end;;

class toStringNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toString" (new toStringHelper)
end;;
