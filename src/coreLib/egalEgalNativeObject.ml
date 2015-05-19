
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class egalEgalNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new BoolExpressionObject.boolExpressionObject (fl == (obj#returnNum()))
  method evalNOD obj = obj 
end;;

class egalEgalStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new BoolExpressionObject.boolExpressionObject (if String.compare x (obj#returnString()) == 0 then true else false)
  method evalNOD obj = obj 
end;;

class egalEgalNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class egalEgalHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new egalEgalNodHelper defaultValue)
    
  method evalNum obj = (new egalEgalNumHelper (obj#returnNum()))
  method evalString obj = (new egalEgalStringHelper (obj#returnString()))
  method evalNOD obj = (new egalEgalNodHelper obj)
end;;

class egalEgalNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_==" (new egalEgalHelper)
end;;
