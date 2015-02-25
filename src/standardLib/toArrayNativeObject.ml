

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toArrayHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalList obj = (new ArrayExpressionObject.arrayExpressionObject (obj#returnList()))
  method evalArray obj = obj
  method evalString obj = 
    let result = ref [] in 
    let str = obj#returnString() in 
    for i = (String.length str) - 1 downto 0 do
      result := (new CharExpressionObject.charExpressionObject str.[i])::!result
    done;
    (new ArrayExpressionObject.arrayExpressionObject !result)
end;;

class toArrayNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toArray" (new toArrayHelper)
end;;
