
exception NoMatch

class matchExpressionObject exprObj matchs =
object
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    let value = exprObj#eval env in
    let rec helper matchs = match (matchs) with
	([pattern],expr)::suite -> 
	  (try
	     let bool = pattern#matchToExpression env value in
	     if bool then
	       expr
	     else 
	       helper suite
	   with _ -> helper suite)
      | _ -> 
	raise NoMatch
    in
    let result = helper matchs in
    env#removeLevel();
    result

  method preEval env idList = 
    let pExprObj = exprObj#preEval env idList in
    let pMatchs = List.map (fun (patterns,expr) -> 
      let ids = List.fold_left (fun acc pattern -> acc@pattern#getIds()) idList patterns in
      let pExpr = expr#preEval env ids in
      (patterns,pExpr)) matchs
    in
    ((new matchExpressionObject pExprObj pMatchs) :> 
	AbstractExpressionObject.abstractExpressionObject)

  method toString() = 
    "match "^(exprObj#toString())^" with "^
      (List.fold_left (fun acc (patterns,expr) -> (if (String.compare acc "" == 0) then "" else acc^"| ")^(
	(List.fold_left (fun acc pattern -> (if (String.compare acc "" == 0) then "" else acc^" ")^(pattern#toString())) "" patterns)^
	  "->"^(expr#toString()))) "" matchs)

end;;