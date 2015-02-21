
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
    Debug.funEvalDebug (Printf.sprintf "Env for evaluation for patterns: %s" (env#toString()));
    let exprToEval = helper lambdas in
    Debug.funEvalDebug (Printf.sprintf "Env for evaluation after associations: %s" (env#toString()));
    Debug.funEvalDebug (Printf.sprintf "Expression to evaluate: %s" (exprToEval#toString()));
    let exprResult = exprToEval#eval env in
    Debug.funEvalDebug (Printf.sprintf "Expression evaluated (and result!): %s" (exprResult#toString()));
    exprResult
end;;
