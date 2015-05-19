

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toBoolHelper =
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalNum obj = new BoolExpressionObject.boolExpressionObject (if (obj#returnNum() == 0.0) then false else true)
  method evalBool obj = obj
  method evalString obj = 
    if (String.compare "true" (obj#returnString()) == 0) then 
      (new BoolExpressionObject.boolExpressionObject true)
    else if (String.compare "false" (obj#returnString()) == 0) then
      (new BoolExpressionObject.boolExpressionObject false)
    else
      (self#evalDefault obj)
end;;

class toBoolNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toBool?" (new toBoolHelper)
end;;
