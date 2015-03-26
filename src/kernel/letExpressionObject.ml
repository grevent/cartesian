
exception LetMatchingException

class letExpressionObject assigns exprObject =
object
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    env#addLevel();
    
    List.iter (fun (pattern,expr) -> 
      let value = expr#eval env in
      if not (pattern#matchToExpression env value) then
	raise LetMatchingException;
    )
      assigns;
    let result = exprObject#eval env in
    env#removeLevel();
    result

  method preEval env idList = 
    let ids = List.fold_left (fun acc (pattern,expr) -> acc@(pattern#getIds())) idList assigns in
    let pAssigns = List.map
      (fun (pattern,expr) -> 
	let (_,nextExpr) = expr#preEval env ids in
	(pattern,nextExpr) )
      assigns
    in
    let (_,finalExpr) = exprObject#preEval env ids in 
    (idList,((new letExpressionObject pAssigns finalExpr) :> AbstractExpressionObject.abstractExpressionObject))
      
  method toString() = 
    "let "^(List.fold_left (fun acc (pattern,expr) -> (if (String.compare "" acc) == 0 then "" else acc^" and ")^(pattern#toString())^" = "^(expr#toString())) "" assigns)^" in "^(exprObject#toString())

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<letExpressionObject>"^
	(List.fold_left (fun acc (pattern,expr) -> acc^(pattern#toXml(n-1))^(expr#toXml(n-1))) "" assigns)^
	(exprObject#toXml(n-1))^
	"</letExpressionObject>"
      
end;;
