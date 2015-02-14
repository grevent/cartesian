
class objectExpressionObject (defs: (string*((AbstractExpressionObject.abstractExpressionObject AbstractPatternObject.abstractPatternObject) list)*(AbstractExpressionObject.abstractExpressionObject)) list) =
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    let obj = new ObjectObject.objectObject in
    
    List.iter (fun (id,patterns,expr) -> 
      obj#addAttribute id 
	(if List.length patterns > 0 then
	    (new FunctionExpressionObject.functionExpressionObject [(patterns,expr)])
	 else
	    (expr#eval env) ) ) defs;
    
    new ObjectWrapperExpressionObject.objectWrapperExpressionObject obj

  method toString() = 
    "{ "^(List.fold_left (fun acc (attribute,patterns,expr) -> 
      acc^attribute^
	(List.fold_left (fun acc pattern -> acc^(pattern#toString())^" ") "" patterns)^"= "^(expr#toString())) "" defs)^" }"

end;;
