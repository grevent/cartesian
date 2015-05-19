
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class puissNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (fl ** (obj#returnNum()))
  method evalNOD obj = obj 
end;;

class puissNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class puissHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new puissNodHelper defaultValue)
    
  method evalNum obj = (new puissNumHelper (obj#returnNum()))
  method evalNOD obj = (new puissNodHelper obj)
end;;

class puissNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_^" (new puissHelper)
end;;
