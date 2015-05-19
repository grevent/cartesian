

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class intHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    let tmp = obj#returnNum() in
    if (mod_float tmp 1.0) > 0.0 then
      new BoolExpressionObject.boolExpressionObject false
    else
      new BoolExpressionObject.boolExpressionObject true;
      
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class intNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "int?" (new intHelper)
end;;
