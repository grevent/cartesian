
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class minFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (min fl (obj#returnFloat()))
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (min fl (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class minIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (min (float_of_int x) (obj#returnFloat()))
  method evalInt obj = new IntExpressionObject.intExpressionObject (min x (obj#returnInt()))
  method evalNOD obj = obj
end;;

class minCharHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalChar obj = new CharExpressionObject.charExpressionObject (min x (obj#returnChar()))
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
    
  method evalFloat obj = (new minFloatHelper (obj#returnFloat()))
  method evalInt obj = (new minIntHelper (obj#returnInt()))
  method evalChar obj = (new minCharHelper (obj#returnChar()))
  method evalString obj = (new minStringHelper (obj#returnString()))
  method evalNOD obj = (new minNodHelper obj)
end;;

class minNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "min" (new minHelper)
end;;
