
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class notEgalNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new BoolExpressionObject.boolExpressionObject (fl != (obj#returnNum()))
  method evalNOD obj = obj 
end;;

class notEgalStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new BoolExpressionObject.boolExpressionObject (if String.compare x (obj#returnString()) != 0 then true else false)
  method evalNOD obj = obj 
end;;

class notEgalNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class notEgalHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new notEgalNodHelper defaultValue)
    
  method evalNum obj = (new notEgalNumHelper (obj#returnNum()))
  method evalString obj = (new notEgalStringHelper (obj#returnString()))
  method evalNOD obj = (new notEgalNodHelper obj)
end;;

class notEgalNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_!=" (new notEgalHelper)
end;;
