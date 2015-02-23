
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class lorIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject ((lor) x (obj#returnInt()))
  method evalNOD obj = obj
end;;

class lorNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;

class lorHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new lorNodHelper defaultValue)
    
  method evalInt obj = (new lorIntHelper (obj#returnInt()))
  method evalNOD obj = (new lorNodHelper obj)
end;;

class lorNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "lor" (new lorHelper)
end;;
