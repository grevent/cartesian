
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class logicalOrBoolHelper x = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval env obj = 
    if x then 
      new BoolExpressionObject.boolExpressionObject true
    else
      let vl = obj#eval env in
      if vl#isBool() then
	vl
      else
	defaultValue
end;;

class logicalOrNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class logicalOrHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new logicalOrNodHelper defaultValue)
    
  method evalBool obj = (new logicalOrBoolHelper (obj#returnBool()))
  method evalNOD obj = (new logicalOrNodHelper obj)
end;;

class logicalOrNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_||" (new logicalOrHelper)
end;;
