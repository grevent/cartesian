
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class prErrAction str = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parents =
    (prerr_string str);

  method preExec env idList =
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject))
    
  method toString() = 
    ("prerr "^str)

  method toXml x = 
    match x with
      0 -> "..."
    | n -> "<prErrAction>"^str^"</prErrAction>"

end;;

class prErrHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
  
  method evalInt obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
  method evalFloat obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
  method evalList obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
  method evalBool obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
  method evalChar obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
  method evalObj obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
  method evalFunction obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
  method evalArray obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
  method evalAction obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
  method evalString obj = (new ActionWrapperExpr.actionWrapperExpr (new prErrAction (obj#toString())))
end;;

class prErrNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "prErr" (new prErrHelper)
end;;
