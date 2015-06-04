
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class absHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
    
  method evalNum obj =
    new NumExpressionObject.numExpressionObject (ComplexTools._abs (obj#returnNum()))
	
  method evalMatrix obj =
    new NumExpressionObject.numExpressionObject (MatrixTools._abs (obj#returnMatrix()))
end;;

class absNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "abs" (new absHelper)
end;;
