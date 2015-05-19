
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class modNumHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (mod_float x (obj#returnNum()))
  method evalNOD obj = obj
end;;

class modNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class modHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new modNodHelper defaultValue)
    
  method evalNum obj = (new modNumHelper (obj#returnNum()))
  method evalNOD obj = (new modNodHelper obj)
end;;

class modNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_mod" (new modHelper)
end;;
