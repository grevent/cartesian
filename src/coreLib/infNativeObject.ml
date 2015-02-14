
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class infFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject (fl < (obj#returnFloat()))
  method evalInt obj = new BoolExpressionObject.boolExpressionObject (fl < (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class infIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new BoolExpressionObject.boolExpressionObject ((float_of_int x) < (obj#returnFloat()))
  method evalInt obj = new BoolExpressionObject.boolExpressionObject (x < (obj#returnInt()))
  method evalNOD obj = obj
end;;

class infStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalString obj = new BoolExpressionObject.boolExpressionObject (if String.compare x (obj#returnString()) < 0 then true else false)
  method evalNOD obj = obj 
end;;

class infNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class infHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new infNodHelper defaultValue)
    
  method evalFloat obj = (new infFloatHelper (obj#returnFloat()))
  method evalInt obj = (new infIntHelper (obj#returnInt()))
  method evalString obj = (new infStringHelper (obj#returnString()))
  method evalNOD obj = (new infNodHelper obj)
end;;

class infNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_<" (new infHelper)
end;;
