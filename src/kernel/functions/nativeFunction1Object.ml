
open AbstractExpressionObject
open NativeFunctionHelper
open NativeFunctionObject
open AbstractFunctionObject

exception WrongParameterAmount

class nativeFunction1Object stringRepresentation (mapping: abstractExpressionObject nativeFunctionHelper) =
object(self)
  inherit [abstractExpressionObject] abstractFunctionObject
    
  method apply env lst = 
    Debug.debugStd "nativeFunction1Object(in)" "apply";
    let result = 
      match lst with
	[] -> (new nativeFunctionObject (self :> abstractExpressionObject abstractFunctionObject))
      | [obj] -> 
	let objEval = obj#eval env in
	mapping#eval env objEval
      | _ -> raise WrongParameterAmount
    in
    Debug.debugStd "nativeFunction1Object(out)" "apply";
    result

  method preEval env idList = 
    Debug.debugStd "nativeFunction1Object(in/out)" "preEval";
    (idList,(self :> abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))

  method toTree() = 
    CartesianTree.NATIVEFUNCTION

end;;
