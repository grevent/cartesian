
exception WrongParameterAmount
  
let defaultValue = new NodExpressionObject.nodExpressionObject;;

let evalIf env obj1 obj2 obj3 = 
  let obj1Eval = obj1#eval env in 
  if obj1Eval#isBool() then
    if obj1Eval#returnBool() then
      (obj2#eval env)
    else
      (obj3#eval env)
  else
    defaultValue

class ifNativePartial2Object obj1 obj2 = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractFunctionObject.abstractFunctionObject 

  method toString() = "(_if "^(obj1#toString())^" "^(obj2#toString())^")"
    
  method apply env lst = 
    match lst with
      [] -> (new NativeFunctionObject.nativeFunctionObject (self :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))
    | [obj3] -> (evalIf env obj1 obj2 obj3 )
    | _ -> raise WrongParameterAmount

end;;

class ifNativePartial1Object obj1 = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractFunctionObject.abstractFunctionObject 
    
  method toString() = "(_if "^(obj1#toString())^")"
    
  method apply env lst = 
    match lst with
      [] -> (new NativeFunctionObject.nativeFunctionObject (self :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))
    | [obj2] -> (new NativeFunctionObject.nativeFunctionObject (new ifNativePartial2Object obj1 obj2))
    | [obj2; obj3] -> 
      (evalIf env obj1 obj2 obj3 )
    | _ -> raise WrongParameterAmount
      
end;;

class ifNativeObject = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractFunctionObject.abstractFunctionObject 
    
  method toString() = "_if"
    
  method apply env lst = 
    match lst with
      [] -> (new NativeFunctionObject.nativeFunctionObject (self :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))
    | [obj] -> (new NativeFunctionObject.nativeFunctionObject (new ifNativePartial1Object obj))
    | [obj1; obj2] -> (new NativeFunctionObject.nativeFunctionObject (new ifNativePartial2Object obj1 obj2))
    | [obj1; obj2; obj3] -> 
      (evalIf env obj1 obj2 obj3 )
    | _ -> raise WrongParameterAmount

end;;
