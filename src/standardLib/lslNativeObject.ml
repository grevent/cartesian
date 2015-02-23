
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class lslIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject ((lsl) x (obj#returnInt()))
  method evalNOD obj = obj
end;;

class lslNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;

class lslHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new lslNodHelper defaultValue)

  method evalInt obj = (new lslIntHelper (obj#returnInt()))
  method evalNOD obj = (new lslNodHelper obj)
end;;

class lslNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "lsl" (new lslHelper)
end;;
