
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class floorHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject (obj#returnInt())
  method evalFloat obj = new IntExpressionObject.intExpressionObject (int_of_float (floor (obj#returnFloat())))
end;;

class floorNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "floor" (new floorHelper)
end;;
