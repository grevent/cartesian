
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class maxFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (max fl (obj#returnFloat()))
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (max fl (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class maxIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (max (float_of_int x) (obj#returnFloat()))
  method evalInt obj = new IntExpressionObject.intExpressionObject (max x (obj#returnInt()))
  method evalNOD obj = obj
end;;

class maxCharHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalChar obj = new CharExpressionObject.charExpressionObject (max x (obj#returnChar()))
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
    
  method evalFloat obj = (new maxFloatHelper (obj#returnFloat()))
  method evalInt obj = (new maxIntHelper (obj#returnInt()))
  method evalChar obj = (new maxCharHelper (obj#returnChar()))
  method evalString obj = (new maxStringHelper (obj#returnString()))
  method evalNOD obj = (new maxNodHelper obj)
end;;

class maxNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "max" (new maxHelper)
end;;
