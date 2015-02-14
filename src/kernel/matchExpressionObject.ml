
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

  method toString() = 
    "match "^(exprObj#toString())^" with "^
      (List.fold_left (fun acc (patterns,expr) -> (if (String.compare acc "" == 0) then "" else acc^"| ")^(
	(List.fold_left (fun acc pattern -> (if (String.compare acc "" == 0) then "" else acc^" ")^(pattern#toString())) "" patterns)^
	  "->"^(expr#toString()))) "" matchs)

end;;
