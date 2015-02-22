
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class cosHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = new FloatExpressionObject.floatExpressionObject (cos (float_of_int (obj#returnInt())))
  method evalFloat obj = new FloatExpressionObject.floatExpressionObject (cos (obj#returnFloat()))
end;;

class cosNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "cos" (new cosHelper)
end;;
