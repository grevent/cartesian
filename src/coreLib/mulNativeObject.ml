
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class mulNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (fl *. (obj#returnNum()))
  method evalNOD obj = obj
end;;

class mulBoolHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalBool obj = new BoolExpressionObject.boolExpressionObject (if (x == false) || (obj#returnBool() == false) then false else true)
  method evalNOD obj = obj 
end;;

class mulNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class mulHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new mulNodHelper defaultValue)
    
  method evalNum obj = (new mulNumHelper (obj#returnNum()))
  method evalBool obj = (new mulBoolHelper (obj#returnBool()))
  method evalNOD obj = (new mulNodHelper obj)
end;;

class mulNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_*" (new mulHelper)
end;;
