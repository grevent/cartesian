
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class subNumHelper fl =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (fl -. (obj#returnNum()))
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
    
  method evalNum obj = (new subNumHelper (obj#returnNum()))
  method evalBool obj = (new subBoolHelper (obj#returnBool()))
  method evalNOD obj = (new subNodHelper obj)
end;;
  
class subNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_-" (new subHelper)
end;;

