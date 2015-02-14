
class forActionObject id exprObj actionObj =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractActionObject.abstractActionObject
    
  method exec parents = 
    let expr = exprObj#eval (Env.newEnv parents) in
    
    if expr#isList() then
      begin
	let lst = expr#returnList() in 
	
	List.iter (fun x -> 
	  (new AssignActionObject.assignActionObject id x)#exec parents;
	  actionObj#exec parents;
	) lst;
	
      end
    else if expr#isArray() then
      begin
	let arr = expr#returnArray() in 
	
	Array.iter (fun x -> 
	  (new AssignActionObject.assignActionObject id x)#exec parents;
	  actionObj#exec parents;
	) arr;
      end
    else
      raise RuntimeObject.NotAListNorAVector

  method toString() = 
    "for "^id^" in "^(exprObj#toString())^" DO "^(actionObj#toString())
    
end;;
