
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class getListHelper lst =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = 
    try
      List.nth lst (obj#returnInt())
    with _-> 
      defaultValue
end;;

class getArrayHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalInt obj = 
    try 
      x.(obj#returnInt())
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
    
  method evalArray obj = (new getArrayHelper (obj#returnArray()))
  method evalList obj = (new getListHelper (obj#returnList()))
  method evalNOD obj = (new getNodHelper obj)
end;;

class getNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_get" (new getHelper)
end;;
