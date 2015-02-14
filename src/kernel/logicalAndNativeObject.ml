
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class logicalAndBoolHelper x = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval env obj = 
    if not x then 
      new BoolExpressionObject.boolExpressionObject false
    else
      let vl = obj#eval env in
      if vl#isBool() then
	vl
      else
	defaultValue
end;;

class logicalAndNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method noEval() = true
  method evalDefaultWOEval env obj = x
end;;
    
class logicalAndHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new logicalAndNodHelper defaultValue)
    
  method evalBool obj = (new logicalAndBoolHelper (obj#returnBool()))
  method evalNOD obj = (new logicalAndNodHelper obj)
end;;

class logicalAndNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_&&" (new logicalAndHelper)
end;;
