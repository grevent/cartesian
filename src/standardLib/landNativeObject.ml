
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class landNumHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    new NumExpressionObject.numExpressionObject ((float_of_int ((land) x (obj#returnNumAsInt()))),0.0)
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
    
  method evalNum obj = (new landNumHelper (obj#returnNumAsInt()))
  method evalNOD obj = (new landNodHelper obj)
end;;

class landNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "land" (new landHelper)
end;;
