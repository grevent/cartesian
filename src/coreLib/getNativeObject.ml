
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class getObjHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj = 
    try
      let vl = obj#returnNumAsInt() in
      x#getAt(vl)
    with _ -> 
      defaultValue
end;;

class getNodHelper x =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method noEval() = true
  method evalDefaultWOEval obj env = x
end;;
    
class getHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new getNodHelper defaultValue)
    
  method evalObj obj = (new getObjHelper (obj#returnObject()))
  method evalNOD obj = (new getNodHelper obj)
end;;

class getNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_get" (new getHelper)
end;;
