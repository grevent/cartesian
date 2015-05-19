
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class asrNumHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (float_of_int ((asr) x (obj#returnNumAsInt())))
  method evalNOD obj = obj
end;;

class asrNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;

class asrHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new asrNodHelper defaultValue)

  method evalNum obj = (new asrNumHelper (obj#returnNumAsInt()))
  method evalNOD obj = (new asrNodHelper obj)
end;;

class asrNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "asr" (new asrHelper)
end;;
