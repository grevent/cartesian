
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class compareFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new IntExpressionObject.intExpressionObject (compare fl (obj#returnFloat()))
  method evalInt obj = new IntExpressionObject.intExpressionObject (compare fl (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class compareIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new IntExpressionObject.intExpressionObject (compare (float_of_int x) (obj#returnFloat()))
  method evalInt obj = new IntExpressionObject.intExpressionObject (compare x (obj#returnInt()))
  method evalNOD obj = obj
end;;

class compareCharHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalChar obj = new IntExpressionObject.intExpressionObject (compare x (obj#returnChar()))
  method evalNOD obj = obj
end;;

class compareStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new IntExpressionObject.intExpressionObject (String.compare x (obj#returnString()))
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
    
  method evalFloat obj = (new compareFloatHelper (obj#returnFloat()))
  method evalInt obj = (new compareIntHelper (obj#returnInt()))
  method evalChar obj = (new compareCharHelper (obj#returnChar()))
  method evalString obj = (new compareStringHelper (obj#returnString()))
  method evalNOD obj = (new compareNodHelper obj)
end;;

class compareNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "compare" (new compareHelper)
end;;
