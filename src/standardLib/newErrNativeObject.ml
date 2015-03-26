
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class newErrAction = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parents =
    prerr_newline()
      
  method preExec env idList =
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject))
      
  method toString() = 
    "newErr"
      
  method toXml x = 
    match x with 
      0 -> "..."
    | _ -> "<newErrAction/>"

end;;

class newErrNativeObject = 
object
  inherit ActionExpressionObject.actionExpressionObject [(new ActionWrapperExpr.actionWrapperExpr (new newErrAction))]
end;;
