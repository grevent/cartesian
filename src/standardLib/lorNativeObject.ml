
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class lorNumHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    if (mod_float x 1.0) > 0.0 then
      raise (AbstractExpressionObject.CanNotConvertToInt (Printf.sprintf "%f" x))
    else
      new NumExpressionObject.numExpressionObject (float_of_int ((lor) (int_of_float x) (obj#returnNumAsInt())));
  method evalNOD obj = obj
end;;

class lorNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;

class lorHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new lorNodHelper defaultValue)
    
  method evalNum obj = (new lorNumHelper (obj#returnNum()))
  method evalNOD obj = (new lorNodHelper obj)
end;;

class lorNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "lor" (new lorHelper)
end;;
