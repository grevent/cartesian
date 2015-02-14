
exception NoCorrespondingPatternForExpr

let rec exprToPattern expr = 
  if (expr#isBool()) then
    (new BoolPatternObject.boolPatternObject (expr#returnBool()))
  else if expr#isFloat() then
    (new FloatPatternObject.floatPatternObject (expr#returnFloat()))
  else if expr#isInt() then
    (new IntPatternObject.intPatternObject (expr#returnInt()))
  else if expr#isChar() then
    (new CharPatternObject.charPatternObject (expr#returnChar()))
  else if expr#isList() then
    (new ListPatternObject.listPatternObject (List.map exprToPattern (expr#returnList())))
  else if expr#isArray() then
    (new ArrayPatternObject.arrayPatternObject (Array.to_list (Array.map exprToPattern (expr#returnArray()))))
  else if expr#isString() then
    (new StringPatternObject.stringPatternObject (expr#returnString()))
  else
    raise NoCorrespondingPatternForExpr
;;

class idPatternObject id = 
object
  inherit [AbstractExpressionObject.abstractExpressionObject] AbstractPatternObject.abstractPatternObject 
    
  method matchToExpression env expr = 
    let exprEval = expr#eval env in
    
    try 
      let idExpr = (env#getOnLevel id) in
      let pattern = exprToPattern idExpr in 
      pattern#matchToExpression env exprEval
    with _ -> 
      env#add id exprEval;
      true

  method toString() = 
    id
	
end;;
