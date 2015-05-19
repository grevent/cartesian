
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class infEgalNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new BoolExpressionObject.boolExpressionObject (fl <= (obj#returnNum()))
  method evalNOD obj = obj 
end;;

class infEgalStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new BoolExpressionObject.boolExpressionObject (if String.compare x (obj#returnString()) <= 0 then true else false)
  method evalNOD obj = obj 
end;;

class infEgalNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class infEgalHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new infEgalNodHelper defaultValue)
    
  method evalNum obj = (new infEgalNumHelper (obj#returnNum()))
  method evalString obj = (new infEgalStringHelper (obj#returnString()))
  method evalNOD obj = (new infEgalNodHelper obj)
end;;

class infEgalNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_<=" (new infEgalHelper)
end;;
