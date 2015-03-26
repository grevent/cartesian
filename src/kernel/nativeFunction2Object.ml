
open AbstractFunctionObject
open AbstractExpressionObject
open NativeFunctionObject
open NativeFunction1Object
open NativeFunctionHelper

exception WrongParameterAmount

class nativeFunction2Object stringRepresentation (mapping: (abstractExpressionObject nativeFunctionHelper) nativeFunctionHelper) =
object(self)
  inherit [abstractExpressionObject] abstractFunctionObject
    
  method toString() = "native "^stringRepresentation
    
  method apply env lst = 
    match lst with
      [] -> (new nativeFunctionObject (self :> abstractExpressionObject abstractFunctionObject))
    | [obj] -> 
      let fn = mapping#eval env obj in
      let tmp = (new nativeFunction1Object ("("^stringRepresentation^" "^(obj#toString())^")") fn) in
 
     new nativeFunctionObject tmp
    | [obj1; obj2] -> 
      let fn1 = mapping#eval env obj1 in 
      (fn1#eval env obj2)
    | _ -> raise WrongParameterAmount

  method preEval env idList = 
    (idList,(self :> abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))

  method toXml x = 
    match x with
      0 -> "..."
    | x -> 
      "<nativeFunction2Object>"^stringRepresentation^"</nativeFunction2Object>"
    
end;;
