
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class printAction str = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parents =
    (print_string str);

  method preExec env idList =
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject))
    
  method toString() = 
    ("print "^str)

end;;

class printHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
  
  method evalInt obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
  method evalFloat obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
  method evalList obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
  method evalBool obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
  method evalChar obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
  method evalObj obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
  method evalFunction obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
  method evalArray obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
  method evalAction obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
  method evalString obj = (new ActionWrapperExpr.actionWrapperExpr (new printAction (obj#toString())))
end;;

class printNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "print" (new printHelper)
end;;
