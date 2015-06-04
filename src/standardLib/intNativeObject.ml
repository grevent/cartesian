

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class intHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    let (re,im) = obj#returnNum() in
    if ((mod_float re 1.0) > 0.0) || ((abs_float im) > 0.0) then
      new BoolExpressionObject.boolExpressionObject false
    else
      new BoolExpressionObject.boolExpressionObject true;
      
  method evalDefault obj = new BoolExpressionObject.boolExpressionObject false
end;;

class intNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "int?" (new intHelper)
end;;
