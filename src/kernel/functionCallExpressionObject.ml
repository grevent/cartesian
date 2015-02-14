
exception WrongArity

class functionCallExpressionObject (functObj: AbstractExpressionObject.abstractExpressionObject) params  = 
object
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env = 
    Debug.funEvalDebug (Printf.sprintf "Function: %s" (functObj#toString()));
    let funct = functObj#eval env in
    Debug.funEvalDebug (Printf.sprintf "Function evaluated: %s" (funct#toString()));    

    if funct#isFunction() then
      begin
	env#addLevel();
	let realFunct = funct#returnFunction() in
	let expr = realFunct#apply env params in
	let result = expr#eval env in
	env#removeLevel();
	result
      end
    else
      raise RuntimeObject.NotAFunction

  method toString() = 
    "( "^(functObj#toString())^(List.fold_left (fun acc param -> acc^(if (String.compare acc "") == 0 then "" else " ")^(param#toString())) "" params)^")"
	
end
