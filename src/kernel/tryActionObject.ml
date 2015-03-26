
exception MoreThanOnePattern

open BasicTools

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

  method preExec env idList = 
    let (_,newObjs) = statefullListMap 
      (fun state x -> (x#preExec env state) ) 
      idList 
      objLst
    in
    let (_,newMatchLst) = statefullListMap 
      (fun state (patterns,expr) -> 
	let ids = (List.fold_left (fun acc pattern -> acc@(pattern#getIds())) state patterns) in
	let (_,newExpr) = expr#preEval env ids in
	(state,(patterns,newExpr)) )
      idList
      matchLst
    in
    (idList,((new tryActionObject newObjs newMatchLst) :> 
	(AbstractExpressionObject.abstractExpressionObject AbstractActionObject.abstractActionObject)))
      
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

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<tryActionObject><actions>...</actions><matchs>...</matchs></tryActionObject>"
    | n -> 
      "<tryActionObject><actions>"^
	(List.fold_left (fun acc x -> acc^(x#toXml(n-2))) "" objLst)^"</actions><matchs>"^
	(List.fold_left (fun acc (patterns,expr) -> acc^
	  (List.fold_left (fun acc pattern -> acc^(pattern#toXml(n-2))) "" patterns)^
	  (expr#toXml(n-2))) "" matchLst)^
	"</matchs></tryActionObject>"
      
end;;
