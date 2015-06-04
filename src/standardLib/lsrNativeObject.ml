
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class lsrIntHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    new NumExpressionObject.numExpressionObject ((float_of_int ((lsr) x (obj#returnNumAsInt()))),0.0)
	
  method evalNOD obj = obj
end;;
  
class lsrNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
							      
  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;

class lsrHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new lsrNodHelper defaultValue)
    
  method evalNum obj = (new lsrIntHelper (obj#returnNumAsInt()))
  method evalNOD obj = (new lsrNodHelper obj)
end;;

class lsrNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "lsr" (new lsrHelper)
end;;
