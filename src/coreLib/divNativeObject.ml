
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class divNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (fl /. (obj#returnNum()))
  method evalNOD obj = obj 
end;;

class divNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class divHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new divNodHelper defaultValue)
    
  method evalNum obj = (new divNumHelper (obj#returnNum()))
  method evalNOD obj = (new divNodHelper obj)
end;;

class divNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_/" (new divHelper)
end;;
