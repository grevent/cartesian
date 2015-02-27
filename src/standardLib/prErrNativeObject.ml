
open NativeFunctionHelper
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

class prErrAction str = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject 
    
  method exec parents =
    (prerr_string str);
    
  method toString() = 
    ("prerr "^str)

end;;

class prErrHelper =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] nativeFunctionHelper defaultValue
  
  method evalInt obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
  method evalFloat obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
  method evalList obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
  method evalBool obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
  method evalChar obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
  method evalObj obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
  method evalFunction obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
  method evalArray obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
  method evalAction obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
  method evalString obj = (new ActionExpressionObject.actionExpressionObject [(new prErrAction (obj#toString()))])
end;;

class prErrNativeObject = 
object
  inherit NativeFunction1Object.nativeFunction1Object "prErr" (new prErrHelper)
end;;
