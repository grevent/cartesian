
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class subFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (fl -. (obj#returnFloat()))
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (fl -. (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class subIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject ((float_of_int x) -. (obj#returnFloat()))
  method evalInt obj = new IntExpressionObject.intExpressionObject (x - (obj#returnInt()))
  method evalNOD obj = obj
end;;

class subBoolHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalBool obj = new BoolExpressionObject.boolExpressionObject (if x == (obj#returnBool()) then false else true)
  method evalNOD obj = obj 
end;;

class subNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class subHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new subNodHelper defaultValue)
    
  method evalFloat obj = (new subFloatHelper (obj#returnFloat()))
  method evalInt obj = (new subIntHelper (obj#returnInt()))
  method evalBool obj = (new subBoolHelper (obj#returnBool()))
  method evalNOD obj = (new subNodHelper obj)
end;;

class subNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_-" (new subHelper)
end;;

