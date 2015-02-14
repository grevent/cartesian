
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class notEgalFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject (fl != (obj#returnFloat()))
  method evalInt obj = new BoolExpressionObject.boolExpressionObject (fl != (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class notEgalIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject ((float_of_int x) != (obj#returnFloat()))
  method evalInt obj = new BoolExpressionObject.boolExpressionObject (x != (obj#returnInt()))
  method evalNOD obj = obj
end;;

class notEgalStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new BoolExpressionObject.boolExpressionObject (if String.compare x (obj#returnString()) != 0 then true else false)
  method evalNOD obj = obj 
end;;

class notEgalNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class notEgalHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new notEgalNodHelper defaultValue)
    
  method evalFloat obj = (new notEgalFloatHelper (obj#returnFloat()))
  method evalInt obj = (new notEgalIntHelper (obj#returnInt()))
  method evalString obj = (new notEgalStringHelper (obj#returnString()))
  method evalNOD obj = (new notEgalNodHelper obj)
end;;

class notEgalNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_!=" (new notEgalHelper)
end;;
