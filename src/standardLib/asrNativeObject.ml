
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class asrIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject ((asr) x (obj#returnInt()))
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

  method evalInt obj = (new asrIntHelper (obj#returnInt()))
  method evalNOD obj = (new asrNodHelper obj)
end;;

class asrNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "asr" (new asrHelper)
end;;
