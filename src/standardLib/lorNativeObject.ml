
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class lorNumHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    new NumExpressionObject.numExpressionObject ((float_of_int ((lor) x (obj#returnNumAsInt()))),0.0)

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
    
  method evalNum obj = (new lorNumHelper (obj#returnNumAsInt()))
  method evalNOD obj = (new lorNodHelper obj)
end;;

class lorNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "lor" (new lorHelper)
end;;
