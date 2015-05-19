
exception WrongArity

class functionCallExpressionObject (functObj: AbstractExpressionObject.abstractExpressionObject) params  = 
object(self)
  inherit AbstractExpressionObject.abstractExpressionObject
    
  method eval env =
    Debug.debugStartMethod self "eval";
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
    Debug.debugEndMethod self "eval" result;
    result

  method preEval env idList =
    Debug.debugStartMethod self "preEval";
    let (idList1,newFunction) = functObj#preEval env idList in 
    let (idList2,newParams) = List.fold_left (fun (idListAcc,paramsAcc) param -> 
      let (resultIdList,paramPreEvaluated) = param#preEval env idListAcc in
      (resultIdList,paramsAcc@[paramPreEvaluated]) )
      (idList1,[]) 
      params 
    in
    let result = ((new functionCallExpressionObject newFunction newParams) :> AbstractExpressionObject.abstractExpressionObject) in
    Debug.debugEndMethod self "preEval" result;
    (idList2,result)

  method toTree() =
    CartesianTree.FUNCTIONCALLEXPRESSION (functObj#toTree(),(List.map (fun x -> x#toTree()) params))
	
end
