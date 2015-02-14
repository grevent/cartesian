
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class mulFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (fl *. (obj#returnFloat()))
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (fl *. (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class mulIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject ((float_of_int x) *. (obj#returnFloat()))
  method evalInt obj = new IntExpressionObject.intExpressionObject (x * (obj#returnInt()))
  method evalString obj = new StringExpressionObject.stringExpressionObject (BasicTools.functional_for 1 x 1 (fun i x -> x^(obj#returnString())) "");
  method evalList obj = new ListWrapperExpressionObject.listWrapperExpressionObject (BasicTools.functional_for 1 x 1 (fun i x -> x@(obj#returnList())) []);
  method evalNOD obj = obj
end;;

class mulStringHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalInt obj = new StringExpressionObject.stringExpressionObject (BasicTools.functional_for 1 (obj#returnInt()) 1 (fun i str -> str^x) "");
  method evalNOD obj = obj 
end;;

class mulBoolHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalBool obj = new BoolExpressionObject.boolExpressionObject (if (x == false) || (obj#returnBool() == false) then false else true)
  method evalNOD obj = obj 
end;;

class mulListHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalInt obj = new ListWrapperExpressionObject.listWrapperExpressionObject (BasicTools.functional_for 1 (obj#returnInt()) 1 (fun i lst -> lst@x) []);
  method evalNOD obj = obj 
end;;

class mulNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class mulHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new mulNodHelper defaultValue)
    
  method evalFloat obj = (new mulFloatHelper (obj#returnFloat()))
  method evalInt obj = (new mulIntHelper (obj#returnInt()))
  method evalString obj = (new mulStringHelper (obj#returnString()))
  method evalBool obj = (new mulBoolHelper (obj#returnBool()))
  method evalList obj = (new mulListHelper (obj#returnList()))
  method evalNOD obj = (new mulNodHelper obj)
end;;

class mulNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_*" (new mulHelper)
end;;
