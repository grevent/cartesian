

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toIntHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalInt obj = obj 
  method evalChar obj = (new IntExpressionObject.intExpressionObject (Char.code (obj#returnChar())))
  method evalFloat obj = (new IntExpressionObject.intExpressionObject (int_of_float (obj#returnFloat())))
  method evalString obj = (new IntExpressionObject.intExpressionObject (int_of_string (obj#returnString())))
end;;

class toIntNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toInt" (new toIntHelper)
end;;
