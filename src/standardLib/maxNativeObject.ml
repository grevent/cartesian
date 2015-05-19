
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class maxNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (max fl (obj#returnNum()))
  method evalNOD obj = obj 
end;;

class maxStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new StringExpressionObject.stringExpressionObject (max x (obj#returnString()))
  method evalNOD obj = obj 
end;;

class maxNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class maxHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new maxNodHelper defaultValue)
    
  method evalNum obj = (new maxNumHelper (obj#returnNum()))
  method evalString obj = (new maxStringHelper (obj#returnString()))
  method evalNOD obj = (new maxNodHelper obj)
end;;

class maxNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "max" (new maxHelper)
end;;
