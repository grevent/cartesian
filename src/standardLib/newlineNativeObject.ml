
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class newlineAction = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parents =
    print_newline()

  method preExec env idList =
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject))
    
  method toString() = 
    "newline"

  method toXml x = 
    match x with
      0 -> "..."
    | _ -> "<newlineAction/>"

end;;

class newlineNativeObject =
object
  inherit ActionExpressionObject.actionExpressionObject [(new ActionWrapperExpr.actionWrapperExpr (new newlineAction))]
end;;
