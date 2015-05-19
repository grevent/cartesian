
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class supEgalNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new BoolExpressionObject.boolExpressionObject (fl >= (obj#returnNum()))
  method evalNOD obj = obj 
end;;

class supEgalStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new BoolExpressionObject.boolExpressionObject (if String.compare x (obj#returnString()) >= 0 then true else false)
  method evalNOD obj = obj 
end;;

class supEgalNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class supEgalHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new supEgalNodHelper defaultValue)
    
  method evalNum obj = (new supEgalNumHelper (obj#returnNum()))
  method evalString obj = (new supEgalStringHelper (obj#returnString()))
  method evalNOD obj = (new supEgalNodHelper obj)
end;;

class supEgalNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_>=" (new supEgalHelper)
end;;
