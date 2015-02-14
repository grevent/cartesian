
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class puissFloatHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (fl ** (obj#returnFloat()))
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (fl ** (float_of_int (obj#returnInt())))
  method evalNOD obj = obj 
end;;

class puissIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject ((float_of_int x) ** (obj#returnFloat()))
  method evalInt obj = new IntExpressionObject.intExpressionObject (int_of_float ((float_of_int x) ** (float_of_int (obj#returnInt()))))
  method evalString obj = new StringExpressionObject.stringExpressionObject (BasicTools.functional_for 1 x 1 (fun i x -> x^(obj#returnString())) "");
  method evalList obj = new ListWrapperExpressionObject.listWrapperExpressionObject (BasicTools.functional_for 1 x 1 (fun i x -> x@(obj#returnList())) []);
  method evalNOD obj = obj
end;;


class puissNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class puissHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new puissNodHelper defaultValue)
    
  method evalFloat obj = (new puissFloatHelper (obj#returnFloat()))
  method evalInt obj = (new puissIntHelper (obj#returnInt()))
  method evalNOD obj = (new puissNodHelper obj)
end;;

class puissNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_^" (new puissHelper)
end;;
