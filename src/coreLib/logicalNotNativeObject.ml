
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class logicalNotHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalBool obj = if (obj#returnBool()) then 
      (new BoolExpressionObject.boolExpressionObject false)
    else      
      (new BoolExpressionObject.boolExpressionObject true)

end;;

class logicalNotNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "_not" (new logicalNotHelper)
end;;
