
exception MoreThanOnePattern

class tryActionObject objLst matchLst =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    try 
      List.iter (fun x -> x#exec parents) objLst;
    with
      RaiseActionObject.CartesianInternalException expr -> 
	let env = (Env.newEnv parents) in
	let expr = (new FunctionObject.functionObject matchLst)#apply env [expr] in
	let result = expr#eval env in
	(result#returnAction())#exec parents;

  method toString() = 
    "try "^
      (List.fold_left (fun acc obj -> acc^(obj#toString())^"; ") "" objLst)^" match "^
      (List.fold_left 
	 (fun acc (patterns,expr) -> 
	   (match patterns with
	     [pattern] -> (if (String.compare acc "" == 0) then "" else (acc^"| "))^(pattern#toString())^" -> "^(expr#toString())
	     | _ -> raise MoreThanOnePattern))
	 "" 
	 matchLst)
	
end;;
