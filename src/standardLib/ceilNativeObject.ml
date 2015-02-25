
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class ceilHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new IntExpressionObject.intExpressionObject (obj#returnInt())
  method evalFloat obj = new IntExpressionObject.intExpressionObject (int_of_float (ceil (obj#returnFloat())))
end;;

class ceilNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "ceil" (new ceilHelper)
end;;
