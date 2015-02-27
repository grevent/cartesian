
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class newErrAction = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parents =
    prerr_newline()
    
  method toString() = 
    "newErr"

end;;

class newErrNativeObject = 
object
  inherit ActionExpressionObject.actionExpressionObject [(new newErrAction)]
end;;
