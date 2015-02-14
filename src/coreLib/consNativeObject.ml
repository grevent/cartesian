
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class consDefaultHelper x = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalList obj = new ListExpressionObject.listExpressionObject (x::(obj#returnList()))
  method evalNOD obj = obj 
end;;

class consNodHelper obj1 = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalDefault obj2 = defaultValue
end;;

class consHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject nativeFunctionHelper] nativeFunctionHelper (new consDefaultHelper defaultValue)
    
  method evalDefault obj = (new consDefaultHelper obj)
  method evalNOD obj = (new consNodHelper obj)
end;;

class consNativeObject = 
object
  inherit NativeFunction2Object.nativeFunction2Object "_::" (new consHelper)
end;;
