
exception NoPatternForFunctionEval of (string*(string list))

class ['expression] functionObject (lambdas: (('expression AbstractPatternObject.abstractPatternObject list)*'expression) list) =
object(self)
  inherit ['expression] AbstractFunctionObject.abstractFunctionObject
    
  method toString() = 
    "lambda "^
      (List.fold_left (fun acc (patterns,expr) -> 
	acc^(if (String.compare acc "") == 0 then "" else "| ")^
	  (List.fold_left (fun acc pattern -> acc^(if (String.compare acc "") == 0 then "" else " ")^(pattern#toString())) "" patterns)^" -> "
	^(expr#toString())) "" lambdas)
      
  method apply (env: 'expression Env.env) (lst: 'expression list) = 
    Debug.stdDebug (self#toXml 3) "apply" "<-" "";
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
    Debug.stdDebug (self#toXml 3) "apply" "->" (exprResult#toXml 3);
    exprResult

  method preEval env idList = 
    Debug.stdDebug (self#toXml 3) "preEval" "<-" "";
    let newLambdas = BasicTools.preEval lambdas env idList in
    let result = (new functionObject newLambdas) in
    Debug.stdDebug (self#toXml 3) "preEval" "->" (result#toXml 3);
    (idList,result)

  method toXml x = 
    match x with
      0 -> "..."
    | x -> 
      "<functionObject>"^
	(List.fold_left (fun acc (patterns,expr) -> acc^(List.fold_left (fun acc pattern -> acc^(pattern#toXml(x-1))) "" patterns)^(expr#toXml(x-1))) "" lambdas)^
	"<functionObject/>"
      
end;;
