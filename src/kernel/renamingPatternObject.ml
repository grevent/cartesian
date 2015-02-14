
class renamingPatternObject pattern id = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    if (pattern#matchToExpression env expr) then
      begin
	env#add id (expr#eval env);
	true
      end
    else
      false

  method toString() = 
    (pattern#toString())^" as "^id

end;;
