
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class egalEgalFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject (fl == (obj#returnFloat()))
  method evalInt obj = new BoolExpressionObject.boolExpressionObject (fl == (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class egalEgalIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject ((float_of_int x) == (obj#returnFloat()))
  method evalInt obj = new BoolExpressionObject.boolExpressionObject (x == (obj#returnInt()))
  method evalNOD obj = obj
end;;

class egalEgalStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new BoolExpressionObject.boolExpressionObject (if String.compare x (obj#returnString()) == 0 then true else false)
  method evalNOD obj = obj 
end;;

class egalEgalNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class egalEgalHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new egalEgalNodHelper defaultValue)
    
  method evalFloat obj = (new egalEgalFloatHelper (obj#returnFloat()))
  method evalInt obj = (new egalEgalIntHelper (obj#returnInt()))
  method evalString obj = (new egalEgalStringHelper (obj#returnString()))
  method evalNOD obj = (new egalEgalNodHelper obj)
end;;

class egalEgalNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_==" (new egalEgalHelper)
end;;
