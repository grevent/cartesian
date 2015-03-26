
open AbstractFunctionObject
open AbstractExpressionObject
open NativeFunctionObject
open NativeFunction1Object
open NativeFunction2Object
open NativeFunctionHelper

exception WrongParameterAmount

class nativeFunction3Object stringRepresentation (mapping: ((abstractExpressionObject nativeFunctionHelper) nativeFunctionHelper) nativeFunctionHelper)
object(self)
  inherit [abstractExpressionObject] abstractFunctionObject
    
  method toString() = "native "^stringRepresentation
    
  method apply env lst = 
    match lst with
      [] -> (new nativeFunctionObject (self :> abstractExpressionObject abstractFunctionObject))
    | [obj] -> 
      let fn = mapping#eval env obj in 
      let tmp = (new nativeFunction2Object ("("^stringRepresentation^" "^(obj#toString())^")") fn) in
	
      new nativeFunctionObject tmp
    | [obj1; obj2] -> 
      let fn1 = mapping#eval env obj1 in 
      let fn2 = fn1#eval env obj2 in
      let tmp = (new nativeFunction1Object ("("^stringRepresentation^" "^(obj1#toString())^" "^(obj2#toString())^")") fn2) in
      
      new nativeFunctionObject tmp
    | [obj1; obj2; obj3 ] -> 
      let fn1 = mapping#eval env obj1 in 
      let fn2 = (fn1#eval env obj2) in
      fn2#eval env obj3
    | _ -> raise WrongParameterAmount
      
  method preEval env idList = 
    (self :> AbstractFunctionObject.abstractFunctionObject)

end;;
