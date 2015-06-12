
exception NoMatch
exception MatchNotWithOnePattern

class matchExpressionObject
	(exprObj: AbstractExpressionObject.abstractExpressionObject)
	(matchs: ((AbstractExpressionObject.abstractExpressionObject AbstractPatternObject.abstractPatternObject list)*(AbstractExpressionObject.abstractExpressionObject)) list) =
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
    env#addLevel();
    let result = helper matchs in
    env#removeLevel();
    result#eval env;

  method preEval env idList = 
    let (pIdList,pExprObj) = exprObj#preEval env idList in
    let pMatchs = List.map (fun (patterns,expr) -> 
      let ids = List.fold_left (fun acc pattern -> acc@pattern#getIds()) idList patterns in
      let (_,pExpr) = expr#preEval env ids in
      (patterns,pExpr)) matchs
    in
    (idList,((new matchExpressionObject pExprObj pMatchs) :> 
		AbstractExpressionObject.abstractExpressionObject))

  method toTree() =
    CartesianTree.MATCHEXPRESSION ((exprObj#toTree()),
		(List.map (fun x -> match x with 
			([pattern],expr) -> ([(pattern#toTree())],(expr#toTree())) |
			_ -> raise MatchNotWithOnePattern
			) matchs) )

end;;
