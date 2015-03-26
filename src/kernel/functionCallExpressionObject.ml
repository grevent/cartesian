
exception WrongArity

class functionCallExpressionObject (functObj: AbstractExpressionObject.abstractExpressionObject) params  = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env = 
    Debug.stdDebug (self#toXml 3) "eval" "<-" "";
    let funct = functObj#eval env in
    let result = if funct#isFunction() then
	begin
	  env#addLevel();
	  let realFunct = funct#returnFunction env in
	  let expr = realFunct#apply env params in
	  let result = expr#eval env in
	  env#removeLevel();
	  result
	end
      else
	new NodExpressionObject.nodExpressionObject
    in
    Debug.stdDebug (self#toXml 3) "eval" "->" (result#toXml 3);
    result

  method preEval env idList = 
    Debug.stdDebug (self#toXml 3) "preEval" "<-" "";
    let (idList1,newFunction) = functObj#preEval env idList in 
    let (idList2,newParams) = List.fold_left (fun (idListAcc,paramsAcc) param -> 
      let (resultIdList,paramPreEvaluated) = param#preEval env idListAcc in
      (resultIdList,paramsAcc@[paramPreEvaluated]) )
      (idList1,[]) 
      params 
    in
    let result = ((new functionCallExpressionObject newFunction newParams) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.stdDebug (self#toXml 3) "preEval" "->" (result#toXml 3);
    (idList2,result)

  method toString() = 
    "("^(functObj#toString())^" "^(List.fold_left (fun acc param -> acc^(if (String.compare acc "") == 0 then "" else " ")^(param#toString())) "" params)^")"

  method toXml x = 
    match x with
      0 -> "..."
    | n -> 
      "<functionCallExpressionObject>"^(functObj#toXml(n-1))^(List.fold_left (fun acc param -> acc^(param#toXml(n-1))) "" params)^"</functionCallExpressionObject>"
	
end
