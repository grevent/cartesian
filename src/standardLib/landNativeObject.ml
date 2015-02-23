
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class landIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject ((land) x (obj#returnInt()))
  method evalNOD obj = obj
end;;

class landNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;

class landHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new landNodHelper defaultValue)
    
  method evalInt obj = (new landIntHelper (obj#returnInt()))
  method evalNOD obj = (new landNodHelper obj)
end;;

class landNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "land" (new landHelper)
end;;
