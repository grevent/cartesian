
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
	let realFunct = funct#returnFunction env in
	let expr = realFunct#apply env params in
	let result = expr#eval env in
	env#removeLevel();
	result
      end
    else
      new NodExpressionObject.nodExpressionObject

  method preEval env idList = 
    let (idList1,newFunction) = functObj#preEval env idList in 
    let (idList2,newParams) = List.fold_left (fun (idListAcc,paramsAcc) param -> 
      let (resultIdList,paramPreEvaluated) = param#preEval env idListAcc in
      (resultIdList,paramsAcc@[paramPreEvaluated]) )
      (idList1,[]) 
      params 
    in
    (idList2,((new functionCallExpressionObject newFunction newParams) :> AbstractExpressionObject.abstractExpressionObject)) 

  method toString() = 
    "("^(functObj#toString())^" "^(List.fold_left (fun acc param -> acc^(if (String.compare acc "") == 0 then "" else " ")^(param#toString())) "" params)^")"
	
end
