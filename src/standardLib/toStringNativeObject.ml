

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toStringHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalNum obj =
    (new StringExpressionObject.stringExpressionObject (ComplexTools.toString (obj#returnNum())))
    
  method evalId obj =
    (new StringExpressionObject.stringExpressionObject (obj#returnId()))
      
  method evalBool obj =
    (new StringExpressionObject.stringExpressionObject (if (obj#returnBool()) then "true" else "false"))
  method evalString obj = obj

  method evalMatrix obj =
    (new StringExpressionObject.stringExpressionObject (MatrixTools.toString (obj#returnMatrix())));
    
end;;

class toStringNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toString" (new toStringHelper)
end;;
