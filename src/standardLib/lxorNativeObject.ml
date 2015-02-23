
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class lxorIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject ((lxor) x (obj#returnInt()))
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
    
  method evalInt obj = (new lxorIntHelper (obj#returnInt()))
  method evalNOD obj = (new lxorNodHelper obj)
end;;

class lxorNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "lxor" (new lxorHelper)
end;;
