
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class divFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (fl /. (obj#returnFloat()))
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (fl /. (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class divIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject ((float_of_int x) /. (obj#returnFloat()))
  method evalInt obj = new IntExpressionObject.intExpressionObject (x / (obj#returnInt()))
  method evalNOD obj = obj
end;;

class divNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class divHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new divNodHelper defaultValue)
    
  method evalFloat obj = (new divFloatHelper (obj#returnFloat()))
  method evalInt obj = (new divIntHelper (obj#returnInt()))
  method evalNOD obj = (new divNodHelper obj)
end;;

class divNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_/" (new divHelper)
end;;
