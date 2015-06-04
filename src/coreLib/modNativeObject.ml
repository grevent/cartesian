
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

exception ModNotPossibleWithComplex
  
class modNumHelper (re,im) = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
							      
  method evalNum obj =
    let (re0,im0) = (obj#returnNum()) in
    if (((abs_float im0) > 0.0) || ((abs_float im) > 0.0)) then
      raise ModNotPossibleWithComplex
    else
      new NumExpressionObject.numExpressionObject ((mod_float re0 re),0.0)
	  
  method evalNOD obj = obj
end;;

class modNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class modHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new modNodHelper defaultValue)
    
  method evalNum obj = (new modNumHelper (obj#returnNum()))
  method evalNOD obj = (new modNodHelper obj)
end;;

class modNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_mod" (new modHelper)
end;;
