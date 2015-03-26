
exception UseCaseNotVerified

class prototypesExpressionObject exprObject useCases = 
object
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env = 
    match env#getMode() with
      Env.Standard -> 
	exprObject#eval env
    | Env.UseCase uc -> 
      let lst = List.filter (fun x -> x#isUC uc) useCases in
      (match lst with
	[] -> raise InstancesExpressionObject.UseCaseNotPrototyped
      | _ -> 
	let result = exprObject#eval env in
	if (List.fold_left (fun acc x -> if x#verifyValue result then acc else false) true lst) then
	  result
	else
	  raise UseCaseNotVerified)
	
  method toString() = 
    "( "^(exprObject#toString())^(List.fold_left (fun acc uc -> acc^": "^(uc#toString())) "" useCases)^")"

  method preEval env idList = 
    let (newIds,newExpr) = exprObject#preEval env idList in
    
    (newIds,((new prototypesExpressionObject newExpr useCases) 
	     :> AbstractExpressionObject.abstractExpressionObject))

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<prototypesExpressionObject>"^(exprObject#toXml(n-1))^
	(List.fold_left (fun acc x -> acc^(x#toXml(n-1))) "" useCases)^
	"</prototypesExpressionObject>"
      
end;;
  
