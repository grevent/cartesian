

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toNumHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalNum obj = obj
			 
  method evalString obj =
    (new NumExpressionObject.numExpressionObject (ComplexTools.parse (obj#returnString())))
end;;

class toNumNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toNum" (new toNumHelper)
end;;
