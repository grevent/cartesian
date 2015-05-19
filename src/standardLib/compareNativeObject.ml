
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class compareNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (float_of_int (compare fl (obj#returnNum())))
  method evalNOD obj = obj 
end;;

class compareStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new NumExpressionObject.numExpressionObject (float_of_int (String.compare x (obj#returnString())))
  method evalNOD obj = obj 
end;;

class compareNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class compareHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new compareNodHelper defaultValue)
    
  method evalNum obj = (new compareNumHelper (obj#returnNum()))
  method evalString obj = (new compareStringHelper (obj#returnString()))
  method evalNOD obj = (new compareNodHelper obj)
end;;

class compareNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "compare" (new compareHelper)
end;;
