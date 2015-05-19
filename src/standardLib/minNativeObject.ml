
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class minNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (min fl (obj#returnNum()))
  method evalNOD obj = obj 
end;;

class minStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
							      
  method evalString obj = new StringExpressionObject.stringExpressionObject (min x (obj#returnString()))
  method evalNOD obj = obj 
end;;

class minNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class minHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new minNodHelper defaultValue)
    
  method evalNum obj = (new minNumHelper (obj#returnNum()))
  method evalString obj = (new minStringHelper (obj#returnString()))
  method evalNOD obj = (new minNodHelper obj)
end;;
  
class minNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "min" (new minHelper)
end;;
