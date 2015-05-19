
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class supNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new BoolExpressionObject.boolExpressionObject (fl > (obj#returnNum()))
  method evalNOD obj = obj 
end;;

class supStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new BoolExpressionObject.boolExpressionObject (if String.compare x (obj#returnString()) > 0 then true else false)
  method evalNOD obj = obj 
end;;

class supNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class supHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new supNodHelper defaultValue)
    
  method evalNum obj = (new supNumHelper (obj#returnNum()))
  method evalString obj = (new supStringHelper (obj#returnString()))
  method evalNOD obj = (new supNodHelper obj)
end;;

class supNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_>" (new supHelper)
end;;
