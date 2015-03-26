
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
    
  method toXml x = 
    match x with
      0 -> "..."
    | n -> "<ifNativePartial2Object>"^(obj1#toXml(n-1))^(obj2#toXml(n-1))^"</ifNativePartial1Object>"

  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))

  method apply env lst = 
    match lst with
      [] -> (new NativeFunctionObject.nativeFunctionObject (self :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))
    | [obj3] -> (evalIf env obj1 obj2 obj3 )
    | _ -> raise WrongParameterAmount

  method preEval env idList = 
    let (idList1,newObj1) = obj1#preEval env idList in
    let (idList2,newObj2) = obj2#preEval env idList1 in
    (idList2,((new ifNativePartial2Object newObj1 newObj2) :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))

end;;

class ifNativePartial1Object obj1 = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractFunctionObject.abstractFunctionObject 
    
  method toXml x = 
    match x with
      0 -> "..."
    | n -> "<ifNativePartial1Object>"^(obj1#toXml(n-1))^"</ifNativePartial1Object>"

  method toString() = "(_if "^(obj1#toString())^")"
    
  method apply env lst = 
    match lst with
      [] -> (new NativeFunctionObject.nativeFunctionObject (self :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))
    | [obj2] -> (new NativeFunctionObject.nativeFunctionObject (new ifNativePartial2Object obj1 obj2))
    | [obj2; obj3] -> 
      (evalIf env obj1 obj2 obj3 )
    | _ -> raise WrongParameterAmount
      
  method preEval env idList = 
    let (idList1,newObj1) = obj1#preEval env idList in
    (idList1,((new ifNativePartial1Object newObj1) :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))
      
end;;

class ifNativeObject = 
object(self)
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractFunctionObject.abstractFunctionObject 
    
  method toString() = "_if"

  method toXml x = 
    match x with
      0 -> "..."
    | _ -> "<ifNativeObject/>"
    
  method preEval env idList = 
    (idList,(self :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))
    
  method apply env lst = 
    match lst with
      [] -> (new NativeFunctionObject.nativeFunctionObject (self :> AbstractExpressionObject.abstractExpressionObject AbstractFunctionObject.abstractFunctionObject))
    | [obj] -> (new NativeFunctionObject.nativeFunctionObject (new ifNativePartial1Object obj))
    | [obj1; obj2] -> (new NativeFunctionObject.nativeFunctionObject (new ifNativePartial2Object obj1 obj2))
    | [obj1; obj2; obj3] -> 
      (evalIf env obj1 obj2 obj3 )
    | _ -> raise WrongParameterAmount

end;;
