
class arrayPatternObject lst =
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in

    if exprEval#isObject() then
      (List.fold_left2 (fun acc x y -> if (x#matchToExpression env y) then acc else false) true lst (exprEval#returnObjectAsList()))
    else
      false

  method getIds() = 
    (List.fold_left (fun acc x -> (x#getIds())@acc) [] lst)

  method toTree() =
    CartesianTree.LISTPATTERN (List.map (fun x -> x#toTree()) lst)

end;;
