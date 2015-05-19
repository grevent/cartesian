
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class cosHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = new NumExpressionObject.numExpressionObject (cos (obj#returnNum()))
end;;

class cosNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "cos" (new cosHelper)
end;;
