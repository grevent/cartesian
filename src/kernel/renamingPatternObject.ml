
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

  method getIds () = 
    id::(pattern#getIds())

  method toString() = 
    (pattern#toString())^" as "^id

  method toXml x = 
    match x with
      0 -> "..."
    | 1 -> "<renamingPatternObject>...<id>"^id^"</id></renamingPatternObject>"
    | n -> "<renamingPatternObject>"^(pattern#toXml(n-2))^"<id>"^id^"</id></renamingPatternObject>"

end;;
