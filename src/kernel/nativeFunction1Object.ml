
open AbstractExpressionObject
open NativeFunctionHelper
open NativeFunctionObject
open AbstractFunctionObject

exception WrongParameterAmount

class nativeFunction1Object stringRepresentation (mapping: abstractExpressionObject nativeFunctionHelper) =
object(self)
  inherit [abstractExpressionObject] abstractFunctionObject
    
  method toString() = "native "^stringRepresentation
    
  method apply env lst = 
    Debug.stdDebug "nativeFunction1Object" "apply" "<-" "";
    let result = 
      match lst with
	[] -> (new nativeFunctionObject (self :> abstractExpressionObject abstractFunctionObject))
      | [obj] -> 
	let objEval = obj#eval env in
	mapping#eval env objEval
      | _ -> raise WrongParameterAmount
    in
    Debug.stdDebug (self#toXml 3) "apply" "->" (result#toXml(3));
    result

  method preEval env idList = 
    Debug.stdDebug (self#toXml 3) "preEval" "<->" (self#toXml(3));
    (idList,(self :> abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))
	
  method toXml x = 
    match x with
      0 -> "..."
    | x -> 
      "<nativeFunction1Object>"^stringRepresentation^"</nativeFunction1Object>"
      
end;;
