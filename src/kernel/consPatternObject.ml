
class consPatternObject car cdr =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject
    
  method matchToExpression env expr =
    let exprEval = expr#eval env in

    if exprEval#isList() then
      match (exprEval#returnList()) with
	hd::tl -> 
	  begin 
	    (car#matchToExpression env hd) &&
	      (cdr#matchToExpression env (new ListWrapperExpressionObject.listWrapperExpressionObject tl))
	  end;
      | [] -> 
	false
    else
      false

  method toString() = 
    (car#toString())^"::"^(cdr#toString())

end;;
