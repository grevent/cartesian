
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class lxorIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    new NumExpressionObject.numExpressionObject ((float_of_int ((lxor) x (obj#returnNumAsInt()))),0.0)
	
  method evalNOD obj = obj
end;;

class lxorNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
  
class lxorHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new lxorNodHelper defaultValue)
    
  method evalNum obj = (new lxorIntHelper (obj#returnNumAsInt()))
  method evalNOD obj = (new lxorNodHelper obj)
end;;

class lxorNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "lxor" (new lxorHelper)
end;;
