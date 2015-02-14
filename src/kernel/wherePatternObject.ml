
class wherePatternObject pattern guardExpr = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    if (pattern#matchToExpression env expr) then
      begin
	let guard = guardExpr#eval env in
	if (guard#isBool()) && (guard#returnBool()) then
	  true
	else
	  false
      end
    else
      false

  method toString() = 
    (pattern#toString())^" where "^(guardExpr#toString())
	
end;;
