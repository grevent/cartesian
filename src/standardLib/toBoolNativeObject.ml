

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toBoolHelper =
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalNum obj =
    let (re,im) = obj#returnNum() in 
    new BoolExpressionObject.boolExpressionObject (if (re == 0.0 && im == 0.0) then false else true)
	
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
