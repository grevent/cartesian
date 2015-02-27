
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class printAction str = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parents =
    (print_string str);
    
  method toString() = 
    ("print "^str)

end;;

class printHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
  
  method evalInt obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
  method evalFloat obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
  method evalList obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
  method evalBool obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
  method evalChar obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
  method evalObj obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
  method evalFunction obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
  method evalArray obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
  method evalAction obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
  method evalString obj = (new ActionExpressionObject.actionExpressionObject [(new printAction (obj#toString()))])
end;;

class printNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "print" (new printHelper)
end;;
