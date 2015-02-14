
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
    match lst with
      [] -> (new nativeFunctionObject (self :> abstractExpressionObject abstractFunctionObject))
    | [obj] -> mapping#eval env obj 
    | _ -> raise WrongParameterAmount
	
end;;
