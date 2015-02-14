
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class supEgalFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject (fl >= (obj#returnFloat()))
  method evalInt obj = new BoolExpressionObject.boolExpressionObject (fl >= (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class supEgalIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject ((float_of_int x) >= (obj#returnFloat()))
  method evalInt obj = new BoolExpressionObject.boolExpressionObject (x >= (obj#returnInt()))
  method evalNOD obj = obj
end;;

class supEgalStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new BoolExpressionObject.boolExpressionObject (if String.compare x (obj#returnString()) >= 0 then true else false)
  method evalNOD obj = obj 
end;;

class supEgalNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class supEgalHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new supEgalNodHelper defaultValue)
    
  method evalFloat obj = (new supEgalFloatHelper (obj#returnFloat()))
  method evalInt obj = (new supEgalIntHelper (obj#returnInt()))
  method evalString obj = (new supEgalStringHelper (obj#returnString()))
  method evalNOD obj = (new supEgalNodHelper obj)
end;;

class supEgalNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_>=" (new supEgalHelper)
end;;
