

open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class toListHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue

  method evalList obj = obj
  method evalArray obj = (new ListExpressionObject.listExpressionObject (Array.to_list (obj#returnArray())))
  method evalString obj = 
    let result = ref [] in 
    let str = obj#returnString() in 
    for i = (String.length str) - 1 downto 0 do
      result := (new CharExpressionObject.charExpressionObject str.[i])::!result
    done;
    (new ListExpressionObject.listExpressionObject !result)
end;;

class toListNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "toList" (new toListHelper)
end;;
