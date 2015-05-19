
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class floorHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (floor (obj#returnNum()))
end;;

class floorNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "floor" (new floorHelper)
end;;
