
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class modIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject (x mod (obj#returnInt()))
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
    
  method evalInt obj = (new modIntHelper (obj#returnInt()))
  method evalNOD obj = (new modNodHelper obj)
end;;

class modNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_mod" (new modHelper)
end;;
