
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class newlineAction = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parents =
    print_newline()
    
  method toString() = 
    "newline"

end;;

class newlineNativeObject = 
object
  inherit ActionExpressionObject.actionExpressionObject [(new newlineAction)]
end;;
