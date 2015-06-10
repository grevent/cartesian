
exception NoPatternForFunctionEval of (string*(string list))

class ['expression] functionObject (lambdas: (('expression AbstractPatternObject.abstractPatternObject list)*'expression) list) =
object(self)
  inherit ['expression] AbstractFunctionObject.abstractFunctionObject
    
  method apply (env: 'expression Env.env) (lst: 'expression list) = 
    Debug.debugStartMethod self "apply";
    let rec helper lambdaList = match lambdaList with
	[] -> 
	  raise (NoPatternForFunctionEval ((self#toString()),(List.map (fun x -> x#toString()) lst)))
      | (patterns,expr)::suite -> 
	try
	  let bools = List.map2 (fun pattern param -> pattern#matchToExpression env param) patterns lst in
	  
	  if List.fold_left (fun acc bool -> if bool then acc else false) true bools then
	    expr
	  else
	    helper suite
	with _ -> helper suite 
    in
    let exprToEval = helper lambdas in
    let exprResult = exprToEval#eval env in
    Debug.debugEndMethod self "apply" exprResult;
    exprResult
      
  method preEval env idList = 
    Debug.debugStartMethod self "preEval";
    let newLambdas = BasicTools.preEval lambdas env idList in
    let result = (new functionObject newLambdas) in
    Debug.debugEndMethod self "preEval" result;
    (idList,result)

  method toTree() = 
    CartesianTree.FUNCTION (List.map (fun (patterns,expr) -> (List.map (fun x -> x#toTree()) patterns),(expr#toTree())) lambdas)

end;;
